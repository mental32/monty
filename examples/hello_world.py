import monty

HELLO_WORLD = """
from builtins import print

def main():
    print("Hello, World!")
"""

code = monty.compile(HELLO_WORLD)

print(code.disassemble())
