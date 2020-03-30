import ast
from typing import TypeVar, Set, Any, Dict, Optional, Type
from types import MappingProxyType
from pathlib import Path
from contextlib import suppress
from dataclasses import dataclass, field

from .ir import Module, Scope, Function, Argument, ArgKind, RawType
from .errors import CompilationError, MissingTypeAnnotation, BadReturnType

__all__ = (
    "BaseVisitor",
    "UnsupportedNode",
    "FunctionVisitor",
    "ReturnVisitor",
    "AssignVisitor",
    "NameVisitor",
    "ConstantVisitor",
    "BinOpVisitor",
)


class UnsupportedNode(CompilationError):
    """Raised when a node does not have a visitor."""


@dataclass
class BaseVisitor(ast.NodeVisitor):
    module: Module
    func: Optional[Function] = field(default=None)

    def find_visitor(self, node) -> "BaseVisitor":
        node_type = type(node)

        visitor_type = VISITORS.get(node_type := type(node), None)
        if visitor_type is None:
            raise UnsupportedNode(f"Unsupported node kind! {node_type!r}")

        visitor = visitor_type(module=self.module, func=self.func,)

        return visitor

    def visit_FunctionDef(self, func_node):
        try:
            func = Function.from_ast_node(self.module.qualname, func_node)
        except CompilationError as exc:
            raise exc
        else:
            self.module.add_function(func)
            visitor = FunctionVisitor(module=self.module, func=func)

            try:
                visitor.visit(func_node)
            except UnsupportedNode:
                raise
            except CompilationError:
                pass


@dataclass
class FunctionVisitor(BaseVisitor):
    def visit_FunctionDef(self, func_node):
        if self.func.current_block is None:
            self.func.current_block = self.func.create_block()

        for subnode in func_node.body:
            visitor = self.find_visitor(subnode)
            visitor.visit(node=subnode)


class ReturnVisitor(BaseVisitor):
    def visit_Return(self, node):
        return_block = self.func.create_block()

        self.func.current_block = return_block

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

            assert self.func.current_block is return_block
            assert return_block.ssa_map, repr(return_block)

            return_type = self.func.returns
            ssa_value = max(return_block.ssa_map)

        if return_type != self.func.returns:
            raise BadReturnType(module=self.module, func=self.func, actual=return_type)

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
            raise CompilationError(f"Op not implemented yet! {ast.dump(node)}")
        else:
            func(kind, x=left, y=right)


class AssignVisitor(BaseVisitor):
    def visit_Assign(self, node):
        visitor = self.find_visitor(value := node.value)
        visitor.visit(node=value)

        top_value = max((block := self.func.current_block).ssa_map)
        value_kind = block.ssa_map[top_value]

        for target in node.targets:
            if isinstance(target, ast.Name):
                assert isinstance(
                    target.ctx, ast.Store
                ), f"Target name context was not store for an assignment! {ast.dump(node)}"
                self.func.variables[target.id] = value_kind
            else:
                raise CompilationError(
                    f"Assignment target not supported yet! {ast.dump(target)}"
                )

        block.def_var(target.id, top_value)


class NameVisitor(BaseVisitor):
    def visit_Name(self, node):
        if isinstance(node.ctx, ast.Load):
            value = self.func.current_block.use_var(
                ident=node.id, kind=self.func.variables[node.id]
            )
        else:
            raise CompilationError(
                f"Unsopported context on NameVisitor for node {ast.dump(node)=}"
            )


T = TypeVar("T")
VISITORS: Dict[Type[T], Type[BaseVisitor]] = {
    ast.Return: ReturnVisitor,
    ast.Constant: ConstantVisitor,
    ast.BinOp: BinOpVisitor,
    ast.Assign: AssignVisitor,
    ast.Name: NameVisitor,
}
