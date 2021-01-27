import sys
import re
import traceback
import textwrap
from typing import Dict, List, Tuple
from pathlib import Path

from . import compile
from .driver import MontyDriver
from .item import Item
from .mir.ebb import Ebb

NEWLINE = "\n"
EBB_TEMPLATE = """
{item.kind!s} {{
{body}
}}
""".strip() + "\n"

def debug(driver: MontyDriver):
    mirout: List[Tuple[Item, Ebb]] = list(driver.lower_into_mir())

    if mirout:
        print("Displaying MIR output for module {")

        for (item, ebb) in mirout:
            body = textwrap.indent("\n".join(repr(instr) for block in ebb.blocks for instr in block), prefix="    ")
            print(textwrap.indent(EBB_TEMPLATE.format(item=item, ebb=ebb, body=body), prefix="    "))

        print("}")


if __name__ == "__main__":
    (_, file, *__) = [*sys.argv, None]

    if (p := Path(file)).exists() and p.is_file():
        output = compile(source=p.read_text())
        debug(output)
        sys.exit(0)

    results: Dict[int, MontyDriver] = {}
    n = 0

    while True:
        inp = input(f"[{n}] >>>>> ")

        if inp == "":
            while (part := input("... ")) != "":
                inp += "\n" + part

        elif (match := re.search(r"^_(\d+)", inp)) is not None:
            group = match.group(1)
            assert group.isdigit()

            try:
                ret = eval(inp, globals(), {**locals(), **{f"_{n}": obj for n, obj in results.items()}})
            except Exception:
                traceback.print_exc()
            else:
                print(ret)

            continue

        try:
            output = compile(inp)
        except KeyboardInterrupt:
            raise
        except Exception:
            traceback.print_exc()
            continue
        else:
            results[n] = output

        debug(output)

        n += 1
