import collections
import os
import json
import statistics

import numpy as np

from rich.console import Console
from rich.table import Table
from rich.text import Text

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_ROOT = os.path.join(ROOT, "data", "obelix96")
LANGUAGES = ["C", "C++", "Rust", "Java", "Go", "JavaScript", "Python"]
BASELINE = LANGUAGES[0]


if __name__ == "__main__":
    data = collections.defaultdict(lambda: collections.defaultdict(list))
    for language in LANGUAGES:
        LANGUAGES_ROOT = os.path.join(DATA_ROOT, language)
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

    table = Table(title="Runtimes")
    table.add_column("Benchmark")
    for language in LANGUAGES:
        table.add_column(f"{language} / {BASELINE}")

    missing_benchmarks = []
    for benchmark in benchmarks:
        if benchmark not in data[BASELINE]:
            missing_benchmarks.append(benchmark)
            continue

        baseline = statistics.geometric_mean(
            [e["runtime"] for e in data[BASELINE][benchmark]]
        )

        entries = []
        for language in LANGUAGES:
            if benchmark not in data[language]:
                entries.append("")
            else:
                value = statistics.geometric_mean(
                    [e["runtime"] for e in data[language][benchmark]]
                )
                entries.append(f"{value / baseline:.2f}")
        table.add_row(*(benchmark, *entries))

    for benchmark in missing_benchmarks:
        text = Text(benchmark)
        text.stylize("strike")
        table.add_row(text)

    Console().print(table)
