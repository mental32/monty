def sanity_check(x: int, y: int) -> bool:
    return x == y

def main() -> int:
    x = 1

    while x != 10:
        x = x + 1

    if sanity_check(x, 10):
        x = 0

    return x - 5 if x - x else (1, 2, 99)[-1]
