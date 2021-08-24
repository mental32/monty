from .__monty import extern


@extern
class object:
    """The base class of the class hierarchy.

    When called, it accepts no arguments and returns a new featureless
    instance that has no instance attributes and cannot be given any.
    """


@extern
class type:
    """
    type(object_or_name, bases, dict)
    type(object) -> the object's type
    type(name, bases, dict) -> a new type
    """


@extern
class bool:
    """bool(x) -> bool

    Returns True when the argument x is true, False otherwise.
    The builtins True and False are the only two instances of the class bool.
    The class bool is a subclass of the class int, and cannot be subclassed.
    """


@extern
class int:
    def __add__(self, x: int) -> int:
        ...

    def __sub__(self, x: int) -> int:
        ...

    def __eq__(self, x: int) -> bool:
        ...

    def __ne__(self, x: int) -> bool:
        ...


@extern
class float:
    """
    float(x=0) -> float

    Convert a number or string to an integer, or return 0 if no arguments
    are given.  If x is a number, return x.__float__().
    """


@extern
class str:
    """
    str(object='') -> str
    str(bytes_or_buffer[, encoding[, errors]]) -> str

    Create a new string object from the given object. If encoding or
    errors is specified, then the object must expose a data buffer
    that will be decoded using the given encoding and error handler.
    Otherwise, returns the result of object.__str__() (if defined)
    or repr(object).
    encoding defaults to sys.getdefaultencoding().
    errors defaults to 'strict'.
    """


@extern
class tuple:
    def __getitem__(self, _index: int) -> object:
        ...

    def __setitem__(self, _index: int, _value: object) -> object:
        ...


@extern
def id(__obj: object) -> int:
    ...


@extern
def isinstance(__obj: object, __class_or_tuple: type) -> bool:
    ...
