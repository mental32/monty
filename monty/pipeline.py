import ast
from sys import stderr
from dataclasses import dataclass, field
from threading import Thread
from queue import Queue, Empty

from . import MontyException, CompilationError, MissingTypeAnnotation


@dataclass
class Pipeline:
    exc_channel: Queue
    active: bool = field(init=False, default=False)

    def __enter__(self):
        self.active = True

        self._exc_count = 0
        self._exc_thread = Thread(target=self.exception_listener)
        self._exc_thread.start()

        return self

    def __exit__(self, _, __, ___):
        self.active = False
        self._exc_thread.join()

    # Properties

    @property
    def failed(self) -> bool:
        return self._exc_count > 0

    @property
    def failures(self) -> int:
        return self._exc_count

    # Listeners

    def exception_listener(self):
        while not self.exc_channel.empty() or self.active:
            try:
                exc = self.exc_channel.get_nowait()
            except Empty:
                continue

            exc_kind = type(exc)

            assert issubclass(exc_kind, MontyException)

            if exc_kind is MissingTypeAnnotation:
                print(
                    f"Missing type annotation! {exc.module.name!r} -> {exc.func.name} @ {exc.func.col_offset}:{exc.func.lineno} :: {ast.dump(exc.arg)}",
                    file=stderr,
                )

            elif exc_kind is CompilationError:
                print(f"Generic compilation error! {exc!r}", file=stderr)

    # Public

    def raise_exc(self, exception: Exception):
        self._exc_count += 1
        self.exc_channel.put(exception)
        raise exception
