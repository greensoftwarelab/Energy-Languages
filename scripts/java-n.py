import argparse
import collections
import json
import os

import matplotlib.pyplot as plt
import numpy as np


def main(args):
    experiments_root = os.path.join(args.data_root, "experiments")
    java_n_experiments = [
        e for e in os.listdir(experiments_root) if e.startswith("Java-")
    ]
    data = collections.defaultdict(list)
    for experiment in java_n_experiments:
        n = int(experiment.split("-")[1])
        path = os.path.join(experiments_root, experiment, f"{args.benchmark}.json")
        if not os.path.exists(path):
            continue
        with open(path, "r") as f:
            for line in f:
                data[n].append(json.loads(line))

        x = np.sort(np.array(list(data.keys())))
        runtime = [[r["runtime"] for r in data[n]] for n in x]
        energy = [[r["energy"]["pkg"] for r in data[n]] for n in x]

        y = np.median(runtime, axis=1) / x
        sigma = np.std(runtime, axis=1) / x

        plt.rcParams.update({"text.usetex": True, "font.family": "serif"})
        with plt.style.context("bmh"):
            fig, ax = plt.subplots()
            fig.set_size_inches(10, 7)
            if not args.no_title:
                ax.set_title(f"Runtime per iteration for {args.benchmark} (Java)")

            ax.scatter(x, y, marker=".")
            ax.errorbar(x, y, sigma, linestyle="", elinewidth=1, capsize=2, alpha=0.5)
            ax.set_axisbelow(True)
            ax.set_ylim(0, ax.get_ylim()[1])
            ax.set_ylabel("Time [ms]")
            ax.set_xlabel("Number of iterations")

            plt.savefig(f"java-n.{args.benchmark}.{args.format}", format=args.format)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--data-root", type=str, required=True)
    parser.add_argument("--benchmark", type=str, required=True)
    parser.add_argument("--format", type=str, default="png")
    parser.add_argument("--no-title", action="store_true")
    main(parser.parse_args())
