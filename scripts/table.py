import argparse
import collections
import json
import os
import statistics

from rich.console import Console
from rich.table import Table
from rich.text import Text


def table(title, args, data, means):
    table = Table(title=title)
    table.add_column("Benchmark")
    for language in args.languages:
        table.add_column(
            f"{language} / {args.baseline}" if language != args.baseline else language
        )

    benchmarks = sorted(list({b for l in data.values() for b in l.keys()}))
    missing_benchmarks = []
    for benchmark in benchmarks:
        if benchmark not in data[args.baseline]:
            missing_benchmarks.append(benchmark)
            continue

        entries = []
        for language in args.languages:
            if benchmark not in data[language]:
                entries.append("")
            elif language == args.baseline:
                entries.append(f"{means[language][benchmark]:.2f}")
            else:
                normalized = (
                    means[language][benchmark] / means[args.baseline][benchmark]
                )
                entries.append(f"{normalized:.2f}")
        table.add_row(*(benchmark, *entries))

    for benchmark in missing_benchmarks:
        text = Text(benchmark)
        text.stylize("strike")
        table.add_row(text)

    per_language_baseline = statistics.geometric_mean(
        [m for m in means[args.baseline].values()]
    )

    per_language_normalized_means = {
        language: statistics.geometric_mean(
            [
                means[language][b] / means[args.baseline][b]
                for b in benchmarks
                if b in means[language] and b in means[args.baseline]
            ]
        )
        for language in args.languages
    }

    table.add_row()
    table.add_row(
        "Geometric Mean",
        *[f"{per_language_normalized_means[l]:.2f}" for l in args.languages],
    )

    return table


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

    Console().print(
        table(
            "Runtime",
            args,
            data,
            {
                language: {
                    benchmark: statistics.geometric_mean(
                        [e["runtime"] for e in subdata]
                    )
                    for benchmark, subdata in data[language].items()
                }
                for language in args.languages
            },
        )
    )

    Console().print(
        table(
            "Energy consumption",
            args,
            data,
            {
                language: {
                    benchmark: statistics.geometric_mean(
                        [e["energy"]["pkg"] for e in subdata]
                    )
                    for benchmark, subdata in data[language].items()
                }
                for language in args.languages
            },
        )
    )


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
    parser.add_argument("--baseline", type=str, default="C")
    main(parser.parse_args())
