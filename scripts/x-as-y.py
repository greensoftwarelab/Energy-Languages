import collections
import json
import os
import statistics

import numpy as np
from rich.console import Console
from rich.table import Table

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_ROOT = os.path.join(ROOT, "data", "obelix96")
X = "C"
Y = "C++"
LANGUAGES = [X, f"{X} as {Y}"]


if __name__ == "__main__":
    data = collections.defaultdict(lambda: collections.defaultdict(list))
    for language in LANGUAGES:
        LANGUAGES_ROOT = os.path.join(DATA_ROOT, language)
        print(LANGUAGES_ROOT)
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
    benchmarks = [b for b in benchmarks if np.all([b in data[l] for l in LANGUAGES])]

    table = Table(
        title=f"Runtime geometric mean of {X} source benchmarks compiled in {X} and {Y} modes"
    )
    table.add_column("Benchmark")
    table.add_column(f"{X} runtime [ms]")
    table.add_column(f"{Y} runtime [ms]")
    table.add_column("Ratio [%]")

    runtimes = [
        [
            statistics.geometric_mean([r["runtime"] for r in data[language][b]])
            for b in benchmarks
        ]
        for language in LANGUAGES
    ]

    for i, benchmark in enumerate(benchmarks):
        table.add_row(
            benchmark,
            f"{runtimes[0][i]:.0f}",
            f"{runtimes[1][i]:.0f}",
            f"{100 * runtimes[1][i] / runtimes[0][i]:.1f}",
        )

    console = Console()
    console.print(table)
