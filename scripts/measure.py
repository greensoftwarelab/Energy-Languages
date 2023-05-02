import argparse
import os
import subprocess
import sys

from rich.console import Console
from rich.progress import *

ROOT = os.path.join(os.path.dirname(__file__), "..")
RAPL_ROOT = os.path.join(ROOT, "scripts", "RAPL", "build", "rapl")

console = Console()
progress_columns = [
    TextColumn("{task.description}"),
    BarColumn(),
    MofNCompleteColumn(),
]


def main(args):
    for language in args.languages:
        benchmarks = sorted(
            [
                d
                for d in os.listdir(os.path.join(ROOT, language))
                if os.path.isdir(os.path.join(ROOT, language, d))
            ]
        )

        with Progress(*progress_columns, console=console) as progress:
            task = progress.add_task(f"[{language}] Compile", total=len(benchmarks))
            for benchmark in list(benchmarks):
                directory = os.path.join(ROOT, language, benchmark)
                compilation = subprocess.run(
                    ["make", "compile"],
                    cwd=directory,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                )
                if compilation.returncode != 0:
                    benchmarks.remove(benchmark)
                    console.print(compilation.stdout.decode("utf-8"), end="")
                progress.advance(task)

        with Progress(*progress_columns, console=console) as progress:
            total = args.n * len(benchmarks)
            task = progress.add_task(f"[{language}] Run", total=total)

            for benchmark in benchmarks:
                path = os.path.join(os.path.abspath(args.output), language)
                if not os.path.exists(path):
                    os.makedirs(path)

            for i in range(args.n):
                for benchmark in benchmarks:
                    directory = os.path.join(ROOT, language, benchmark)

                    json = os.path.join(
                        os.path.abspath(args.output), language, f"{benchmark}.json"
                    )
                    try:
                        run_status = subprocess.run(
                            ["make", "measure"],
                            cwd=directory,
                            stdout=subprocess.DEVNULL,
                            stderr=subprocess.DEVNULL,
                            env={**os.environ, "JSON": json},
                            timeout=args.timeout,
                        ).returncode

                        if run_status != 0:
                            console.print(
                                f"[{language}] {benchmark}: Run #{i + 1} failed."
                            )
                    except subprocess.TimeoutExpired:
                        console.print(
                            f"[{language}] {benchmark}: Run #{i + 1} timed out."
                        )
                    progress.advance(task)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Runs the measurements.")

    parser.add_argument(
        "--languages",
        required=True,
        metavar="PL",
        nargs="+",
        help="List of PLs to measure. Names must match directory names",
    )
    parser.add_argument(
        "-n",
        required=True,
        type=int,
        help="Number of iterations per language and benchmark",
    )
    parser.add_argument(
        "--output",
        "-o",
        required=True,
        type=str,
        help="Path to output directory",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=None,
        help="Timeout for process execution",
    )

    args = parser.parse_args()

    if args.n > 0 and not os.path.exists(RAPL_ROOT):
        raise "Could not find the RAPL executable. Make sure you build it first."

    main(args)
