import json
import os
import statistics

import matplotlib.pyplot as plt
import numpy as np

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
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
        x = sorted(data.keys())
        y_runtime = [statistics.median([r["runtime"] for r in data[n]]) / n for n in x]

        fig, ax = plt.subplots()
        ax.scatter(x, y_runtime, marker=".")
        ax.grid(which="both")
        ax.set_axisbelow(True)
        ax.set_ylabel("Time [ms]")
        ax.set_xlabel("Number of iterations")
        ax.set_title("Relative time per iteration for the Java binary-trees benchmark")
        plt.savefig(f"{benchmark}.{FORMAT}", format=FORMAT)
