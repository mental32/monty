import ast
from typing import TypeVar, Set, Any, Dict, Optional, Type
from types import MappingProxyType
from pathlib import Path
from contextlib import suppress
from dataclasses import dataclass, field

from . import Module, Scope, Function, Pipeline, Argument, ArgKind, RawType
from .errors import CompilationError, MissingTypeAnnotation, BadReturnType


@dataclass
class BaseVisitor(ast.NodeVisitor):
    pipeline: Pipeline
    module: Module
    func: Optional[Function] = field(default=None)

    # def __getattribute__(self, key):
    #     # if key.startswith("visit_"):
    #     #     print(f"0x{hex(id(self)).upper()[2:]} @ {type(self).__name__} :: {key}")

    #     return object.__getattribute__(self, key)

    def find_visitor(self, node) -> "BaseVisitor":
        node_type = type(node)

        visitor_type = VISITORS.get(node_type := type(node), None)
        if visitor_type is None:
            raise CompilationError(f"Unsupported node kind! {node_type!r}")

        visitor = visitor_type(
            pipeline=self.pipeline, module=self.module, func=self.func,
        )

        return visitor

    def visit_FunctionDef(self, func_node):
        try:
            func = Function.from_ast_node(self.module.qualname, func_node)
        except CompilationError as exc:
            self.pipeline.raise_exc(exc)
        else:
            self.module.add_function(func)
            visitor = FunctionVisitor(
                pipeline=self.pipeline, module=self.module, func=func
            )

            with suppress(CompilationError):
                visitor.visit(func_node)


@dataclass
class FunctionVisitor(BaseVisitor):
    def visit_FunctionDef(self, func_node):
        for subnode in func_node.body:
            visitor = self.find_visitor(subnode)
            visitor.visit(node=subnode)

        print(self.func)


class ReturnVisitor(BaseVisitor):
    def visit_Return(self, node):
        return_block = self.func.create_block()

        if isinstance(node.value, ast.Constant):
            ssa_value = ConstantVisitor.encode_with_block(
                const=node.value, block=return_block
            )
            return_type = return_block.ssa_map[ssa_value]
        else:
            if self.func.current_block is None:
                self.func.current_block = return_block

            visitor = self.find_visitor(return_node := node.value)
            visitor.visit(node=return_node)

            if self.func.current_block is not return_block:
                ssa_value = return_block.add_kwarg(name="value", kind=self.func.returns)

                return_value_block = self.func.current_block
                return_value_block.jump(
                    return_block, kwargs={"value": max(return_value_block.ssa_map)}
                )
                return_type = return_value_block.return_type
            else:
                return_type = self.func.returns
                ssa_value = max(return_block.ssa_map)

        if return_type != self.func.returns:
            raise BadReturnType(module=self.module, func=self.func, actual=return_type)

        print(return_block)

        return_block.return_(ssa_value)


class ConstantVisitor(BaseVisitor):
    @staticmethod
    def encode_with_block(
        *, const: ast.Constant, block: "ir.AbstractBlock"
    ) -> "ir.SSAValue":
        if const.kind is None:
            # Gotta rely on the type(node.value)

            kind = type(const.value)

            if kind is int:
                value = block.iconst(RawType(name="int"), const.value)
            else:
                assert False, f"Not supported! {ast.dump(const)!r}"
        else:
            assert False, "Not implemented!"

        return value

    def visit_Constant(self, node):
        assert (
            self.func.current_block is not None
        ), f"Not focused on any block! {self.func!r}"
        self.encode_with_block(const=node, block=self.func.current_block)


class BinOpVisitor(BaseVisitor):
    def visit_BinOp(self, node):
        block = self.func.current_block

        # Is the current block reusable? (Disabled for now)
        if False and (block is not None) and (block.instructions or block.ssa_map):
            block = self.func.create_block()  # No, create a fresh one.
            self.func.current_block = block

        results = {node.left: None, node.right: None}
        for subnode in [node.left, node.right]:
            visitor = self.find_visitor(subnode)
            visitor.visit(node=subnode)

            assert self.func.current_block is block
            results[subnode] = max(self.func.current_block.ssa_map)

        left, right = results[node.left], results[node.right]

        assert left is not None
        assert right is not None

        if block.ssa_map[left] != block.ssa_map[right]:
            raise CompilationError("Bad types for BinOp")

        kind = block.ssa_map[left]

        match = {
            ast.Sub: block.sub,
            ast.Add: block.add,
            ast.Mult: block.mul,
        }

        try:
            func = match[type(node.op)]
        except KeyError:
            self.pipeline.raise_exc(CompilationError(f"Op not implemented yet! {ast.dump(node)}"))
        else:
            func(kind, x=left, y=right)

T = TypeVar("T")
VISITORS: Dict[Type[T], Type[BaseVisitor]] = {
    ast.Return: ReturnVisitor,
    ast.Constant: ConstantVisitor,
    ast.BinOp: BinOpVisitor,
}
