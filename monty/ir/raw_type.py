from dataclasses import dataclass, field
from typing import Type, TypeVar, Optional

T = TypeVar("T")


@dataclass
class RawType:
    """A Typical DST discriminant."""

    name: str
    impl: Optional[T] = field(default=None)
