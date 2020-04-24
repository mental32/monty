"""MIRI - Monty's Mid-level IR Interpreter."""
import argparse

from .machine import Machine


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("source")
    args = parser.parse_args()

    with open(args.source) as inf:
        pass


if __name__ == "__main__":
    main()
