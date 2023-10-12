import argparse
import collections
import json
import math
import os
import statistics

import matplotlib.patches as mpatches
import matplotlib.pyplot as plt


def main(args):
    data = collections.defaultdict(lambda: collections.defaultdict(list))
    for language in args.languages:
        LANGUAGES_ROOT = os.path.join(args.data_root, language)
        if not os.path.isdir(LANGUAGES_ROOT):
            args.languages.remove(language)
            print(f"Warning: {LANGUAGES_ROOT} does not exist, skipping")
            continue
        for benchmark in os.listdir(LANGUAGES_ROOT):
            path = os.path.join(LANGUAGES_ROOT, benchmark)
            assert os.path.isfile(path) and path.endswith(".json")
            benchmark = benchmark[:-5]
            with open(path, "r") as file:
                for line in file:
                    line = json.loads(line)
                    data[language][benchmark].append(line)

    benchmarks = sorted(list({b for l in data.values() for b in l.keys()}))

    plt.rcParams.update({"text.usetex": True, "font.family": "serif"})
    with plt.style.context("bmh"):
        markers = [".", "v", "^", "<", ">", "s", "*", "x", "D", "2", "+"]
        colors = plt.rcParams["axes.prop_cycle"].by_key()["color"]

        runtimes = {
            language: {
                benchmark: 0.001
                * statistics.geometric_mean(
                    [r["runtime"] for r in data[language][benchmark]]
                )
                for benchmark in benchmarks
                if benchmark in data[language]
            }
            for language in args.languages
        }

        energies = {
            language: {
                benchmark: 0.001
                * statistics.geometric_mean(
                    [r["energy"]["pkg"] for r in data[language][benchmark]]
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
                f"Energy consumed as a function of runtime for all (language, benchmark) pairs"
            )
        ax.set_xlabel("Time [s]")
        ax.set_ylabel("Energy [kJ]")
        if args.axin_xmax:
            axins = ax.inset_axes([0.02, 0.48, 0.5, 0.5])

        max_x = 0
        axin_max_x = 0
        axin_max_y = 0
        for i, language in enumerate(args.languages):
            for j, benchmark in enumerate(benchmarks):
                if benchmark not in data[language]:
                    continue
                if runtimes[language][benchmark] <= args.xmax:
                    max_x = max(max_x, runtimes[language][benchmark])
                    ax.plot(
                        runtimes[language][benchmark],
                        energies[language][benchmark],
                        markersize=4,
                        color=colors[i],
                        marker=markers[j],
                    )
                if args.axin_xmax and runtimes[language][benchmark] <= args.axin_xmax:
                    axin_max_x = max(axin_max_x, runtimes[language][benchmark])
                    axin_max_y = max(axin_max_y, energies[language][benchmark])
                    axins.plot(
                        runtimes[language][benchmark],
                        energies[language][benchmark],
                        markersize=4,
                        color=colors[i],
                        marker=markers[j],
                    )

        ax.set_xlim(0, min(1.1 * max_x, ax.get_xlim()[1]))
        ax.set_ylim(0, ax.get_ylim()[1])
        ax.set_axisbelow(True)

        if args.axin_xmax:
            axins.set_xlim(0, min(1.1 * axin_max_x, axins.get_xlim()[1]))
            axins.set_ylim(0, axins.get_ylim()[1])
            axins.set_axisbelow(True)
            axins.set_xticklabels([])
            axins.set_yticklabels([])
            ax.indicate_inset_zoom(axins)

        languages_legend_handles = [
            mpatches.Patch(color=colors[i], label=language.replace("#", "\\#"))
            for i, language in enumerate(args.languages)
        ]

        whitespace_handle = mpatches.Patch(color="none", label="")

        benchmark_legend_handles = [
            ax.scatter(
                [],
                [],
                color=plt.rcParams["axes.edgecolor"],
                marker=markers[i],
                label=benchmark,
            )
            for i, benchmark in enumerate(benchmarks)
        ]

        ax.legend(
            handles=languages_legend_handles
            + [whitespace_handle]
            + benchmark_legend_handles,
            prop={"size": 9},
        )

        fig.tight_layout()
        plt.savefig(f"time_v_energy.{args.format}", format=args.format)


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
    parser.add_argument("--xmax", type=int, default=math.inf)
    # Indicating this enables the inset axis.
    parser.add_argument("--axin-xmax", type=int, default=None)
    parser.add_argument("--no-title", action="store_true")
    main(parser.parse_args())
