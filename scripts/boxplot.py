import collections
import json
import os
import statistics

import matplotlib.pyplot as plt
import numpy as np

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_ROOT = os.path.join(ROOT, "data", "obelix96")
LANGUAGES = ["C", "C++", "Rust"]
# LANGUAGES = ["JavaScript", "TypeScript"]
FORMAT = "png"


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
    benchmarks = [b for b in benchmarks if np.all([b in data[l] for l in LANGUAGES])]

    normalizations = [
        statistics.geometric_mean([r["runtime"] for r in data[LANGUAGES[0]][b]])
        for b in benchmarks
    ]

    data = {
        language: {
            benchmark: [
                r["runtime"] / normalizations[i] for r in data[language][benchmark]
            ]
            for i, benchmark in enumerate(benchmarks)
        }
        for language in LANGUAGES
    }

    with plt.style.context("bmh"):
        fig, ax = plt.subplots()
        fig.set_size_inches(12, 6)

        d = 2  # Horizontal distance between benchmarks
        w = 1  # Width for a single benchmark.

        colors = plt.rcParams["axes.prop_cycle"].by_key()["color"]

        for i, benchmark in enumerate(benchmarks):
            m = i * d
            x = np.linspace(m - w / 2, m + w / 2, len(LANGUAGES))

            for j, language in enumerate(LANGUAGES):
                y = data[language][benchmark]
                ax.scatter(np.repeat(x[j], len(y)), y, color=colors[j], s=5, alpha=0.3)

        ax.set_xticks(
            [i * d for i in range(len(benchmarks))], labels=benchmarks, rotation=30
        )
        ax.set_xticks(
            [(2 * i - 1) * d / 2 for i in range(1, len(benchmarks))], minor=True
        )
        ax.grid(axis="y", visible=False)
        ax.grid(axis="x", which="major", visible=False)
        ax.grid(axis="x", which="minor", visible=True)
        # ax.set_ylim(0, ax.get_ylim()[1])
        ax.set_ylim(0, 6)
        ax.set_xlim(-d / 2, (len(benchmarks) - 1) * d + d / 2)
        ax.set_ylabel(f"Time (Normalized to {LANGUAGES[0]})")
        ax.set_title(
            f"Comparing benchmark runtimes for {', '.join(LANGUAGES[:-1])} and {LANGUAGES[-1]}"
        )

        for j, language in enumerate(LANGUAGES):
            ax.plot([], [], color=colors[j], label=language)
        ax.legend()

        fig.tight_layout()
        plt.savefig(f"boxplot.{FORMAT}", format=FORMAT)
