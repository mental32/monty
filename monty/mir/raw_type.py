from dataclasses import dataclass, field
from typing import Type, TypeVar, Optional

T = TypeVar("T")

__all__ = ("RawType",)


@dataclass
class RawType:
    """A Typical DST discriminant."""

    name: str
    impl: Optional[T] = field(default=None)
