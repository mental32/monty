import monty

HELLO_WORLD = """
def main():
    print('Hello, World!')
"""

code = monty.compile(HELLO_WORLD)

print(code.disassemble())
