import collections
import json
import os
import statistics

import matplotlib
import matplotlib.patches as mpatches
import matplotlib.pyplot as plt


ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_ROOT = os.path.join(ROOT, "data", "obelix96")
LANGUAGES = [
    "C",
    "C++",
    "Rust",
    "Java",
    "Go",
    "C#",
    "JavaScript",
    "TypeScript",
    "Python",
]
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

    with plt.style.context("bmh"):
        benchmarks = sorted(list({b for l in data.values() for b in l.keys()}))
        markers = matplotlib.markers.MarkerStyle.filled_markers
        colors = plt.rcParams["axes.prop_cycle"].by_key()["color"]

        runtimes = {
            language: {
                benchmark: statistics.geometric_mean(
                    [r["runtime"] for r in data[language][benchmark]]
                )
                for benchmark in benchmarks
                if benchmark in data[language]
            }
            for language in LANGUAGES
        }

        energies = {
            language: {
                benchmark: statistics.geometric_mean(
                    # TODO: Hack because of some faulty data. Can remove >= 0 in the future.
                    [
                        r["energy"]["pkg"]
                        for r in data[language][benchmark]
                        if r["energy"]["pkg"] >= 0
                    ]
                )
                for benchmark in benchmarks
                if benchmark in data[language]
            }
            for language in LANGUAGES
        }

        fig, ax = plt.subplots()
        fig.set_size_inches(10, 7)
        ax.set_title(
            f"Energy consumed as a function of runtime for all (language, benchmark) pairs"
        )
        ax.set_xlabel("Time [ms]")
        ax.set_ylabel("Energy [J]")
        axins = ax.inset_axes([0.02, 0.48, 0.5, 0.5])
        for i, language in enumerate(LANGUAGES):
            for j, benchmark in enumerate(benchmarks):
                if benchmark not in data[language]:
                    continue
                ax.plot(
                    runtimes[language][benchmark],
                    energies[language][benchmark],
                    # markersize=4,
                    color=colors[i],
                    marker=markers[j],
                )
                axins.plot(
                    runtimes[language][benchmark],
                    energies[language][benchmark],
                    # markersize=4,
                    color=colors[i],
                    marker=markers[j],
                )

        ax.set_xlim(0, ax.get_xlim()[1])
        ax.set_ylim(0, ax.get_ylim()[1])
        ax.set_axisbelow(True)

        axins.set_axisbelow(True)
        axins.set_xlim(0, 9000)
        axins.set_ylim(0, 800)
        axins.set_xticklabels([])
        axins.set_yticklabels([])
        ax.indicate_inset_zoom(axins)

        language_legend = ax.legend(
            handles=[
                mpatches.Patch(color=colors[i], label=language)
                for i, language in enumerate(LANGUAGES)
            ],
        )
        benchmark_legend = ax.legend(
            handles=[
                ax.scatter(
                    [],
                    [],
                    color=plt.rcParams["axes.edgecolor"],
                    marker=markers[i],
                    label=benchmark,
                )
                for i, benchmark in enumerate(benchmarks)
            ],
        )
        ax.add_artist(language_legend)
        ax.add_artist(benchmark_legend)

        fig.tight_layout()
        plt.savefig(f"time_v_energy.{FORMAT}", format=FORMAT)
