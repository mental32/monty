import ast
import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import List

from . import Scope, Function

__all__ = ("Module",)


@dataclass
class Module:
    """A Python module."""

    qualname: str
    file_path: Path
    imports: List[str] = field(default_factory=list)
    scope: Scope = field(default_factory=Scope)

    @property
    def name(self) -> str:
        return self.qualname.rsplit(".", maxsplit=1)[-1]

    def add_function(self, func: Function):
        self.scope[func.name] = func

    def into_raw(self, refs) -> str:
        blob = {
            "qualname": self.qualname,
            "file_path": f"{self.file_path}",
            "imports": self.imports.copy(),
            "scope": {
                key: value.into_raw(refs)
                for key, value in self.scope.names.items()
                if isinstance(value, Function)
            },
        }

        from pprint import pprint

        pprint(blob)
        print(repr(self))

        return blob
