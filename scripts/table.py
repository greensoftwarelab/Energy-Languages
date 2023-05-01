import collections
import os
import json
import statistics

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

    means = {
        language: {
            benchmark: statistics.geometric_mean([e["runtime"] for e in subdata])
            for benchmark, subdata in data[language].items()
        }
        for language in LANGUAGES
    }

    table = Table(title="Runtimes")
    table.add_column("Benchmark")
    for language in LANGUAGES:
        table.add_column(f"{language} / {BASELINE}")

    missing_benchmarks = []
    for benchmark in benchmarks:
        if benchmark not in data[BASELINE]:
            missing_benchmarks.append(benchmark)
            continue

        entries = []
        for language in LANGUAGES:
            if benchmark not in data[language]:
                entries.append("")
            else:
                normalized = means[language][benchmark] / means[BASELINE][benchmark]
                entries.append(f"{normalized:.2f}")
        table.add_row(*(benchmark, *entries))

    for benchmark in missing_benchmarks:
        text = Text(benchmark)
        text.stylize("strike")
        table.add_row(text)

    per_language_baseline = statistics.geometric_mean(
        [m for m in means[BASELINE].values()]
    )
    per_language_normalized_means = {
        language: statistics.geometric_mean([m for m in means[language].values()])
        / per_language_baseline
        for language in LANGUAGES
    }

    table.add_row()
    table.add_row(
        "Geometric Mean",
        *[f"{per_language_normalized_means[l]:.2f}" for l in LANGUAGES],
    )

    Console().print(table)
