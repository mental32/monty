import ast
import json
import sys
from sys import stderr
from threading import Thread
from queue import Queue, Empty
from pathlib import Path

import click
from cranelift.ffi import compile as ccompile

from monty.ir import Module
from monty import (
    BaseVisitor,
    MontyException,
    MissingTypeAnnotation,
    CompilationError,
    Pipeline,
)


@click.command()
@click.argument("source_file", type=click.File("r"))
def main(source_file: click.File):
    tree = ast.parse(source_file.read())

    channel = Queue()
    pipeline = Pipeline(channel)

    with pipeline:
        source_path = Path(source_file.name).absolute()

        assert source_path.exists(), repr(source_path)

        main_module = Module(qualname="__main__", file_path=source_path)

        visitor = BaseVisitor(pipeline=pipeline, module=main_module)
        visitor.visit(tree)

        if not pipeline.failed:
            refs = {"int": "I64"}

            payload = {"modules": {"__main__": main_module.into_raw(refs)}}

            from pprint import pprint

            pprint(payload)

            object_code = ccompile(refs, json.dumps(payload))

            print(object_code)

    if pipeline.failed:
        sys.exit(f"Aborting compilation due to {pipeline.failures!r} errors.")


if __name__ == "__main__":
    main()
