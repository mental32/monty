import monty

HELLO_WORLD = """
def main():
    return
"""

code = monty.compile(HELLO_WORLD)

print(code.disassemble())
