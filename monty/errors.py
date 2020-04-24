class MontyException(Exception):
    """Base class exception for all library errors."""


class CompilationException(MontyException):
    """Base class exception for all compilation-related errors."""


class TypeCheckError(CompilationException):
    """Base class for all type-related errors."""
