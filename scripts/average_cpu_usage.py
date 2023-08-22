import argparse
import collections
import json
import os
import statistics

import matplotlib.pyplot as plt
from rich.console import Console
from rich.table import Table
import scipy


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

    plt.rcParams.update({"text.usetex": True, "font.family": "serif"})
    with plt.style.context("bmh"):
        energy_over_time_ratio = {
            language: {
                benchmark: statistics.geometric_mean(
                    [
                        r["energy"]["pkg"] / (1e-3 * r["runtime"])
                        for r in data[language][benchmark]
                    ]
                )
                for benchmark in benchmarks
                if benchmark in data[language]
            }
            for language in args.languages
        }

        fig, ax = plt.subplots()
        fig.set_size_inches(10, 7)
        if not args.no_title:
            ax.set_title(
                f"Power consumption as a function of CPU usage for all (language, benchmark) pairs"
            )
        ax.set_xlabel("CPU usage")
        ax.set_ylabel("Average power consumption [W]")

        all_xs = []
        all_ys = []
        for language in args.languages:
            x = []
            y = []
            for benchmark in benchmarks:
                if benchmark in energy_over_time_ratio[language]:
                    x.append(cpu_usages[language][benchmark])
                    y.append(energy_over_time_ratio[language][benchmark])
            all_xs.extend(x)
            all_ys.extend(y)
            ax.scatter(
                x,
                y,
                marker=".",
                s=50,
                label=language.replace("#", "\\#"),
            )

        regression = scipy.stats.linregress(all_xs, all_ys)
        print("Regression slope :", regression.slope)
        print("Regression ravlue:", regression.rvalue)
        print("Regression stderr:", regression.stderr)

        ax.legend()
        fig.tight_layout()
        plt.savefig(f"cpu_usage.{args.format}", format=args.format)


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
    parser.add_argument("--format", type=str, default="png")
    parser.add_argument("--no-title", action="store_true")
    main(parser.parse_args())
