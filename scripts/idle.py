import json
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CSV_PATH = os.path.join(ROOT, "data", "obelix96", "idle.csv")
FORMAT = "png"


def is_sorted(l):
    return all(l[i] <= l[i + 1] for i in range(len(l) - 1))


if __name__ == "__main__":
    data = pandas.read_csv(
        CSV_PATH,
        delimiter=",",
        names=["timestamp", "energy"],
        dtype={"timestamp": np.int64, "energy": np.float64},
    )

    timestamp = data.get("timestamp").values
    energy = data.get("energy").values

    assert is_sorted(timestamp)

    x = (timestamp - timestamp[0]) / 1e9
    y = np.cumsum(energy)

    with plt.style.context("bmh"):
        fig, ax = plt.subplots()
        ax.scatter(x, y, marker=".")
        ax.set_axisbelow(True)
        ax.set_ylim(0, ax.get_ylim()[1])
        ax.set_xlabel("Time [s]")
        ax.set_ylabel("Total energy consumed [J]")
        ax.set_title(f"Energy consumed while idle")

        plt.savefig(f"idle.{FORMAT}", format=FORMAT)
