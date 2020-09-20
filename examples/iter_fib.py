import monty

s = """
def fib(n: int) -> int:
    if n == 0:
        return 0
    else:
        n -= 1

    a = 0
    r = 1

    while n != 0:
        t = r
        r += a
        a = t
        n -= 1

    return r


def main() -> int:
    return fib(42)
"""

code = monty.compile(s)

print(code.dissasemble())
