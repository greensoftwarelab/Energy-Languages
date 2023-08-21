import argparse
import collections
import json
import os
import statistics

from rich.console import Console
from rich.table import Table


def main(args):
    data = collections.defaultdict(lambda: collections.defaultdict(list))
    for language in args.languages:
        LANGUAGES_ROOT = os.path.join(args.data_root, language)
        assert os.path.isdir(LANGUAGES_ROOT)
        for benchmark in os.listdir(LANGUAGES_ROOT):
            path = os.path.join(LANGUAGES_ROOT, benchmark)
            assert os.path.isfile(path) and path.endswith(".json")
            benchmark = benchmark[:-5]
            with open(path, "r") as file:
                for line in file:
                    line = json.loads(line)
                    data[language][benchmark].append(line)

    benchmarks = sorted(list({b for l in data.values() for b in l.keys()}))

    table = Table(title=f"Average CPU usage for all (language, benchmark) pairs")
    table.add_column("Benchmark")
    for language in args.languages:
        table.add_column(f"{language}")

    def timeval_to_seconds(tv):
        return tv["tv_sec"] + 1e-6 * tv["tv_usec"]

    def cpu_usage(user_cpu_time, kernel_cpu_time, runtime):
        return (user_cpu_time + kernel_cpu_time) / runtime

    cpu_usages = {
        language: {
            benchmark: statistics.geometric_mean(
                [
                    cpu_usage(
                        timeval_to_seconds(r["rusage"]["ru_utime"]),
                        timeval_to_seconds(r["rusage"]["ru_stime"]),
                        1e-3 * r["runtime"],
                    )
                    for r in data[language][benchmark]
                ]
            )
            for benchmark in benchmarks
            if benchmark in data[language]
        }
        for language in args.languages
    }

    for benchmark in benchmarks:
        row = [benchmark]
        for language in args.languages:
            if benchmark in cpu_usages[language]:
                row.append(f"{cpu_usages[language][benchmark]:.2f}")
            else:
                row.append("")
        table.add_row(*row)

    console = Console()
    console.print(table)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--data-root", type=str, required=True)
    parser.add_argument(
        "--languages",
        type=str,
        nargs="+",
        default=[
            "C",
            "C++",
            "Rust",
            "Go",
            "Java",
            "C#",
            "JavaScript",
            "TypeScript",
            "PHP",
            "Python",
        ],
    )
    main(parser.parse_args())
