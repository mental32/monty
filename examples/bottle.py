"""Bottle of beer on the wall, a recreation of the classic by Guido."""
from sys import argv

if len(argv) >= 2:
    upper = int(argv[1])
else:
    upper = 100

bottles = {
    0: "No more bottles of beer",
    1: "One bottle of beer",
}

for i in range(upper, 0, -1):
    bottle: str = bottles.get(i, f"{i} bottles of beer")
    print(f"{bottle} on the wall,")
    print(f"{bottle}.")
    print("Take one down, pass it around,")
    bottle = bottles.get(i - 1, f"{i - 1} bottles of beer")
    print(f"{bottle} on the wall.")
