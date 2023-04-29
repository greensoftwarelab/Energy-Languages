import json
import os

import matplotlib.pyplot as plt
import numpy as np

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_ROOT = os.path.join(ROOT, "data", "obelix96", "Java-N")
FORMAT = "png"


if __name__ == "__main__":
    benchmarks = list(os.listdir(DATA_ROOT))
    for benchmark in benchmarks:
        files = list(os.listdir(os.path.join(DATA_ROOT, benchmark)))
        data = {}
        for file in files:
            assert file.endswith(".json")
            n = int(file[:-5])
            data[n] = []
            with open(os.path.join(DATA_ROOT, benchmark, file), "r") as fd:
                for line in fd:
                    data[n].append(json.loads(line))

        x = np.sort(np.array(list(data.keys())))
        runtime = list([r["runtime"] for r in data[n]] for n in x)
        energy = list([r["energy"]["pkg"] for r in data[n]] for n in x)
        cycles = list([r["cycles"] for r in data[n]] for n in x)

        y = np.median(runtime, axis=1) / x
        sigma = np.std(runtime, axis=1) / x

        with plt.style.context("bmh"):
            fig, ax = plt.subplots()
            ax.scatter(x, y, marker=".")
            ax.errorbar(x, y, sigma, linestyle="", elinewidth=1, capsize=2, alpha=0.5)
            ax.set_axisbelow(True)
            ax.set_ylim(0, ax.get_ylim()[1])
            ax.set_ylabel("Time [ms]")
            ax.set_xlabel("Number of iterations")
            ax.set_title(f"Java {benchmark}\nRelative time per iteration")

            plt.savefig(f"{benchmark}.{FORMAT}", format=FORMAT)
