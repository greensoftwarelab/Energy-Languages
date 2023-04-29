import collections
import json
import os
import statistics

import matplotlib.pyplot as plt
import numpy as np

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_ROOT = os.path.join(ROOT, "data", "obelix96")
LANGUAGES = ["JavaScript", "TypeScript"]
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

    subdata = [
        [r["runtime"] / normalizations[i] for r in data[language][benchmark]]
        for i, benchmark in enumerate(benchmarks)
        for language in LANGUAGES
    ]

    delta = 0.175
    positions = [i // 2 + (2 * (i % 2) - 1) * delta for i in range(len(subdata))]

    with plt.style.context("bmh"):
        fig, ax = plt.subplots()
        fig.set_size_inches(12, 8)
        bp = ax.boxplot(
            subdata,
            positions=positions,
            widths=(2 * delta - delta / 2),
            showfliers=False,
        )

        for i in range(len(subdata)):
            color = "blue" if i % 2 == 0 else "red"
            plt.setp(bp["boxes"][i], color=color)
            plt.setp(bp["medians"][i], color=color)
            plt.setp(bp["caps"][2 * i], color=color)
            plt.setp(bp["caps"][2 * i + 1], color=color)
            plt.setp(bp["whiskers"][2 * i], color=color)
            plt.setp(bp["whiskers"][2 * i + 1], color=color)

        ax.set_xticks(
            [i for i in range(len(benchmarks))], labels=benchmarks, rotation=90
        )
        # ax.set_ylim(0, ax.get_ylim()[1])
        ax.set_ylabel("Time [ms]")
        ax.set_title(
            f"Comparing C source benchmarks compiled in C and C++ modes\nNormalized to the geometric mean of the C version"
        )

        (blue,) = ax.plot([1, 1], "b-")
        (red,) = ax.plot([1, 1], "r-")
        ax.legend((blue, red), LANGUAGES)
        blue.set_visible(False)
        red.set_visible(False)

        fig.tight_layout()
        plt.savefig(f"c-as-c++.{FORMAT}", format=FORMAT)
