import argparse
import os
import subprocess
import sys

from rich.progress import *

ROOT = os.path.join(os.path.dirname(__file__), "..")


def main(args):
    for language in args.languages:
        benchmarks = [
            d
            for d in os.listdir(os.path.join(ROOT, language))
            if os.path.isdir(os.path.join(ROOT, language, d))
        ]
        benchmarks.sort()
        for benchmark in list(benchmarks):
            directory = os.path.join(ROOT, language, benchmark)
            compilation_status = subprocess.run(
                ["make", "compile"],
                cwd=directory,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            ).returncode
            if compilation_status != 0:
                print(f"[{language}] {benchmark}: Compilation failed.", file=sys.stderr)
                benchmarks.remove(benchmark)

        with Progress(BarColumn(), MofNCompleteColumn(), transient=True) as progress:
            total = args.n * len(benchmarks)
            task = progress.add_task(f"[{language}]", total=total)

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
                    run_status = subprocess.run(
                        ["make", "measure"],
                        cwd=directory,
                        stdout=subprocess.DEVNULL,
                        stderr=subprocess.DEVNULL,
                        env={**os.environ, "JSON": json},
                    ).returncode

                    if run_status != 0:
                        print(
                            f"[{language}] {benchmark}: Run #{i + 1} failed.",
                            file=sys.stderr,
                        )
                    progress.update(task, advance=1)


if __name__ == "__main__":
    if not os.path.exists(os.path.join(ROOT, "scripts", "RAPL", "build", "rapl")):
        raise "Could not find the RAPL executable. Make sure you build it first."

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

    main(parser.parse_args())
