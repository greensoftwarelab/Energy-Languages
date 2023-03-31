import argparse
from collections import defaultdict
import csv
import os
import statistics

from rich.console import Console
from rich.table import Table
from rich.text import Text


def main(args):
    languages = [
        f[:-4]
        for f in os.listdir(args.data)
        if os.path.isfile(os.path.join(args.data, f)) and f.endswith(".csv")
    ]
    languages.sort()
    assert args.baseline in languages

    all_benchmarks = set()

    data = defaultdict(lambda: defaultdict(lambda: []))
    for language in languages:
        with open(os.path.join(args.data, f"{language}.csv"), "r") as f:
            reader = csv.reader(f)
            benchmarks = next(reader)
            all_benchmarks.update(benchmarks)
            for line in reader:
                assert len(line) == len(benchmarks)
                for i in range(len(benchmarks)):
                    line[i] = line[i].strip()
                    if line[i] != "":
                        data[language][benchmarks[i]].append(float(line[i]))

    all_benchmarks = sorted(list(all_benchmarks))

    averages = {
        l: {b: statistics.mean(data[l][b]) for b in all_benchmarks if b in data[l]}
        for l in languages
    }

    median_n = statistics.median(
        [len(r) for language in languages for r in data[language].values()]
    )

    table = Table(title=f"Runtimes")
    table.add_column("Benchmark")
    for language in languages:
        table.add_column(f"{language} / {args.baseline}", justify="right")

    missing_benchmarks = []
    for benchmark in all_benchmarks:
        if benchmark not in data[args.baseline]:
            missing_benchmarks.append(benchmark)
            continue

        entries = []
        for language in languages:
            baseline = averages[args.baseline][benchmark]
            if benchmark not in data[language]:
                entries.append("")
            else:
                normalized = averages[language][benchmark] / baseline
                text = Text(f"{normalized:.2f}")
                if len(data[language][benchmark]) < median_n:
                    text.stylize("red")
                entries.append(text)
        table.add_row(*(benchmark, *entries))

    for benchmark in missing_benchmarks:
        text = Text(benchmark)
        text.stylize("strike")
        table.add_row(text)

    Console().print(table)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Generates a normalized runtime table."
    )

    parser.add_argument(
        "--data",
        type=str,
        required=True,
        metavar="DIRECTORY",
        help="Directory where the CSV files live",
    )
    parser.add_argument(
        "--baseline",
        type=str,
        metavar="PL",
        default="C",
        help="Language to use when normalizing runtimes",
    )

    args = parser.parse_args()

    main(args)