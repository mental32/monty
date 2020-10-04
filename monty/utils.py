from contextlib import contextmanager
from dataclasses import dataclass, field
from typing import Dict, Generic, Any, TypeVar, Optional, Union
from types import MappingProxyType

V = TypeVar("V")


@contextmanager
def swapattr(obj: Any, name: str, default: Any, target: Any):
    """Temporarily swap some objects attribute value with a target."""
    previous = getattr(obj, name, default)
    setattr(obj, name, target)
    yield
    setattr(obj, name, previous)


@dataclass
class SSAMap(Generic[V]):
    """A helper to store unique instances of type `V` mapped to unique `int` keys."""

    __internal: Dict[int, V] = field(repr=False, init=False, default_factory=dict)
    __counter: int = field(repr=False, init=False, default=-1)

    @property
    def mapping(self) -> MappingProxyType:
        return MappingProxyType(self.__internal)

    def insert(self, value: V, clobber: bool = False) -> int:
        """Insert a value into the map and produce an integer key for it.

        By default this insert works in a non clobbering manner, meaning that,
        there will be no duplicate values within the map. When inserting a
        value already present the key for the already existing instance of the
        value is returned.
        """
        if not clobber and (value_id := self.get_by_value(value)) is not None:
            return value_id

        self.__counter = idx = self.__counter + 1
        self.__internal[idx] = value

        return idx

    def set_unchecked(self, key: int, value: V):
        """Set the key `key` equal to the value `value`.

        This will not increment any counters or perform clobbering checks;
        it is semantically equal to `mapping[key] = value`.
        """
        self.__internal[key] = value

    def get(self, key: int) -> Optional[V]:
        """Equal to `dict.get(key, None)`."""
        return self.__internal.get(key, None)

    def get_unchecked(self, key: Union[int, V]) -> Union[V, int]:
        """Gets: the value for a given integer key or the integer key for a given value."""
        if type(key) is int:  # Strict typecheck here since `TypeInfo` is an `IntEnum`
            return self.__internal[key]

        if (value := self.get_by_value(key)) is not None:
            return value

        raise KeyError(f"No entry {key=!r} found in {self.__internal=!r}")

    def get_by_value(self, this: V) -> Optional[int]:
        """Get the integer key for a value."""
        for key, that in self.__internal.items():
            if this == that:
                return key
        else:
            return None

    def __getitem__(self, key: Union[int, V]) -> Union[V, int]:
        return self.get_unchecked(key)

    def __setitem__(self, key: int, value: V):
        assert type(key) is int, "Can only accept integer types as keys!"
        self.set_unchecked(key, value)

    def __iter__(self):
        return iter(self.__internal.items())
