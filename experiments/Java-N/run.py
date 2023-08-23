import argparse
import os
import random
import subprocess

from rich.progress import *

ROOT = os.path.dirname(os.path.abspath(__file__))


def main(args):
    directory = os.path.join(os.path.abspath(args.output), args.benchmark)
    if not os.path.exists(directory):
        os.makedirs(directory)

    with Progress(
        TextColumn("{task.description}"),
        BarColumn(),
        MofNCompleteColumn(),
    ) as progress:
        task = progress.add_task("Total", total=args.n * (args.max - args.min + 1))
        for i in range(args.n):
            for NNNNN in range(args.min, args.max + 1):
                progress.console.log(f"Iteration = {i + 1} / NNNNN = {NNNNN}")

                # Build
                process = subprocess.run(
                    ["make"],
                    cwd=os.path.join(ROOT, args.benchmark),
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                    env={
                        **os.environ,
                        "NNNNN": str(NNNNN),
                        "RANDOMIZED_ENVIRONMENT_OFFSET": "".join(["X"] * random.randint(0, 4096)),
                    },
                )

                process.check_returncode()

                # Run
                process = subprocess.run(
                    ["make", "measure"],
                    cwd=os.path.join(ROOT, args.benchmark),
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                    env={
                        **os.environ,
                        "JSON": os.path.join(directory, f"{NNNNN}.json"),
                    },
                )

                process.check_returncode()

                progress.advance(task)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--benchmark", required=True, type=str)
    parser.add_argument(
        "--min", required=True, type=int, help="Minimum NNNNN (inclusive)"
    )
    parser.add_argument(
        "--max", required=True, type=int, help="Maximum NNNNN (inclusive)"
    )
    parser.add_argument(
        "-n", required=True, type=int, help="Number of times to run for each NNNNN"
    )
    parser.add_argument("--output", "-o", required=True, type=str)
    main(parser.parse_args())
