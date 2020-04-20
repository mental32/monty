from dataclasses import dataclass, field
from typing import List, Dict, Set, Union

__all__ = ("Scope", "Import")


@dataclass
class Import:
    """An import."""
    name: str
    alias: str
    level: int

    def __hash__(self) -> int:
        return hash((self.name, self.alias, self.level))


@dataclass
class Scope:
    """A semantic scope."""

    imports: Set[Import] = field(default_factory=set)
    names: Dict[str, "RawType"] = field(default_factory=dict)

    def __setitem__(self, key: str, value: Union["RawType", "Argument"]):
        self.names[key] = value

    def __getitem__(self, key: str):
        return self.names[key]
