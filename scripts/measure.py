import argparse
import os
import random
import subprocess

from rich.console import Console
from rich.progress import *

ROOT = os.path.join(os.path.dirname(__file__), "..")
RAPL_ROOT = os.path.join(ROOT, "scripts", "RAPL", "build", "rapl")

console = Console(markup=False)
progress_columns = [
    TextColumn("{task.description}"),
    BarColumn(),
    MofNCompleteColumn(),
]


def run_benchmark(language, benchmark, timeout, type, env=os.environ):
    # Todd Mytkowicz, Amer Diwan, Matthias Hauswirth, and Peter F. Sweeney. 2009.
    # Producing wrong data without doing anything obviously wrong!
    # SIGPLAN Not. 44, 3 (March 2009), 265–276. https://doi.org/10.1145/1508284.1508275
    env["RANDOMIZED_ENVIRONMENT_OFFSET"] = "".join(["X"] * random.randint(0, 4096))
    try:
        return subprocess.run(
            ["make", type],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            cwd=os.path.join(ROOT, language, benchmark),
            timeout=timeout,
            env=env,
        ).returncode
    except subprocess.TimeoutExpired:
        return -1


def main(args):
    for language in args.languages:
        benchmarks = sorted(
            [
                d
                for d in os.listdir(os.path.join(ROOT, language))
                if os.path.isdir(os.path.join(ROOT, language, d))
                and os.path.exists(os.path.join(ROOT, language, d, "Makefile"))
            ]
        )

        with Progress(*progress_columns, console=console) as progress:
            task = progress.add_task(f"{language}::Compile", total=len(benchmarks))
            for benchmark in list(benchmarks):
                directory = os.path.join(ROOT, language, benchmark)
                compilation = subprocess.run(
                    ["make", "compile"],
                    cwd=directory,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                )
                if compilation.returncode != 0:
                    console.print(f"{language}::{benchmark}::Compile failed.")
                    benchmarks.remove(benchmark)
                progress.advance(task)

        for benchmark in benchmarks:
            os.makedirs(
                os.path.join(os.path.abspath(args.output), language), exist_ok=True
            )

        for benchmark in benchmarks:
            codes = []
            if args.warmup > 0:
                with Progress(*progress_columns, console=console) as progress:
                    task = progress.add_task(
                        f"{language}::{benchmark}::Warmup", total=args.warmup
                    )
                    for i in range(args.warmup):
                        status = run_benchmark(language, benchmark, args.timeout, "run")
                        if status == -1:
                            console.print(
                                f"{language}::{benchmark} Warmup #{i} timed out."
                            )
                        elif status != 0:
                            console.print(
                                f"{language}::{benchmark} Warmup #{i} failed."
                            )
                        codes.append(status)
                        progress.advance(task)
                if all([code != 0 for code in codes]):
                    print(
                        f"[{language}] [{benchmark}] All warmup runs failed. Skipping."
                    )
                    continue

            if args.n > 0:
                with Progress(*progress_columns, console=console) as progress:
                    task = progress.add_task(
                        f"{language}::{benchmark}::Measure", total=args.n
                    )
                    for i in range(args.n):
                        json = os.path.join(
                            os.path.abspath(args.output), language, f"{benchmark}.json"
                        )

                        env = {**os.environ, "JSON": json}

                        status = run_benchmark(
                            language, benchmark, args.timeout, "measure", env
                        )
                        if status == -1:
                            console.print(
                                f"{language}::{benchmark} Run #{i} timed out."
                            )
                        elif status != 0:
                            console.print(f"{language}::{benchmark} Run #{i} failed.")
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
    parser.add_argument(
        "--warmup",
        type=int,
        default=1,
        help="Number of warmup runs for each benchmark",
    )

    args = parser.parse_args()

    if args.n > 0 and not os.path.exists(RAPL_ROOT):
        raise "Could not find the RAPL executable. Make sure you build it first."

    main(args)
