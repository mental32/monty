from dataclasses import dataclass


@dataclass
class Diagnostic:
    message: str


class Error(Diagnostic):
    pass


class Warning(Diagnostic):
    pass
