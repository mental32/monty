import monty

HELLO_WORLD = """
def oof():
    pass
"""

code = monty.compile(HELLO_WORLD)

print(code.disassemble())
