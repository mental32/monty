from abc import ABC, abstractmethod
from typing import Set

import monty
from monty.errors import TypeCheckError

__all__ = ("TypeInfo",)


class TypeInfo:
    """Base class for all comprehensible types."""

    def reconstruct(self, tcx: "InferenceEngine") -> str:
        """Produce a locally constructed representation of the type."""
        raise NotImplementedError("Failed to implement reconstruct.")

    @abstractmethod
    def size(self) -> int:
        """Get the size of this type in bytes."""
        raise NotImplementedError()
