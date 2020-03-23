from dataclasses import dataclass, field
from typing import List, Dict, Set, Union


@dataclass
class Scope:
    """A semantic scope."""

    imports: Set[str] = field(default_factory=set)
    names: Dict[str, "RawType"] = field(default_factory=dict)

    def __setitem__(self, key: str, value: Union["RawType", "Argument"]):
        self.names[key] = value

    def __getitem__(self, key: str):
        return self.names[key]
