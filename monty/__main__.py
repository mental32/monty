from . import compile

test_input = """
from std.builtins import int

def fib(n: int) -> int:
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)
""".strip()

# def test_input(x: int) -> int:
#     return x

if __name__ == "__main__":
    from . import utils

    _ = compile(test_input)

    for __ in _.lower_into_mir():
        print("EBB")
        for bb in __.blocks:
            print("\n\tBB")
            for instr in bb:
                print(f"\t\t{instr}")
