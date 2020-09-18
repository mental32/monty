from contextlib import contextmanager
from dataclasses import dataclass, field
from typing import Dict, Generic, Any, TypeVar, Optional
from types import MappingProxyType

V = TypeVar("V")


@contextmanager
def swapattr(obj: Any, name: str, default: Any, target: Any):
    previous = getattr(obj, name, default)
    setattr(obj, name, target)
    yield
    setattr(obj, name, previous)


@dataclass
class SSAMap(Generic[V]):
    __internal: Dict[int, V] = field(repr=False, init=False, default_factory=dict)
    __counter: int = field(repr=False, init=False, default=-1)

    @property
    def mapping(self) -> MappingProxyType:
        return MappingProxyType(self.__internal)

    def insert(self, value: V) -> int:
        self.__counter += 1
        self.__internal[self.__counter] = value
        return self.__counter

    def set_unchecked(self, key: int, value: V):
        self.__internal[key] = value

    def get(self, key: int) -> Optional[V]:
        return self.__internal.get(key, None)

    def get_unchecked(self, key: int) -> V:
        return self.__internal[key]

    def get_by_value(self, this: V) -> Optional[int]:
        for key, that in self.__internal.items():
            if this == that:
                return key
        else:
            return None

    def __getitem__(self, key: int) -> V:
        return self.get_unchecked(key)

    def __setitem__(self, key: int, value: V):
        self.set_unchecked(key, value)

    def __iter__(self):
        return iter(self.__internal.items())
