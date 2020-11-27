import monty

from pathlib import Path

source_dir = Path(__file__).parent.joinpath("source_files")

def test_compile_source_files():
    for file in source_dir.iterdir():
        _ = monty.compile(open(file).read())
