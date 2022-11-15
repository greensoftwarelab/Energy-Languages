import argparse
from collections import defaultdict, OrderedDict
import os
import subprocess
import time
import sys

from rich.progress import Progress


# Default values
LANGUAGES = ["C", "C++", "Go", "Java", "JavaScript", "Python"]
N = 100

ROOT = os.path.dirname(__file__)
CSV_SEPARATOR = ","


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
                continue

        runtimes = defaultdict(lambda: [])
        with Progress(transient=True) as progress:
            total = args.n * len(benchmarks)
            task = progress.add_task(f"[{language}]", total=total)

            for i in range(args.n):
                for benchmark in benchmarks:
                    directory = os.path.join(ROOT, language, benchmark)
                    start = time.time()
                    run_status = subprocess.run(
                        ["make", "run"],
                        cwd=directory,
                        stdout=subprocess.DEVNULL,
                        stderr=subprocess.DEVNULL,
                    ).returncode
                    end = time.time()
                    if run_status != 0:
                        print(
                            f"[{language}] {benchmark}: Run #{i + 1} failed.",
                            file=sys.stderr,
                        )
                    else:
                        runtimes[benchmark].append(end - start)
                    progress.update(task, advance=1)

        runtimes = OrderedDict(sorted(runtimes.items()))
        with open(os.path.join(args.output, f"{language}.csv"), "w") as output:
            output.write(CSV_SEPARATOR.join(runtimes.keys()))
            output.write("\n")
            for i in range(args.n):
                output.write(
                    CSV_SEPARATOR.join(
                        [str(r[i]) if i < len(r) else "" for r in runtimes.values()]
                    )
                )
                output.write("\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Runs the measurements.")

    parser.add_argument(
        "--languages",
        nargs="+",
        metavar="PL",
        default=LANGUAGES,
        help="List of PLs to measure. Names must match directory names",
    )
    parser.add_argument(
        "-n",
        type=int,
        default=N,
        help="Number of iterations per language and benchmark",
    )
    parser.add_argument(
        "--output",
        "-o",
        type=str,
        required=True,
        help="Path to output directory. A CSV file will be generated for each language",
    )

    args = parser.parse_args()

    if not os.path.exists(args.output):
        os.makedirs(args.output)

    main(args)
