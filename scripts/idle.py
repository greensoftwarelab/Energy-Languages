import os

import numpy as np
import pandas

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CSV_PATH = os.path.join(ROOT, "data", "obelix96", "idle.csv")


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

    timestamp = timestamp.astype(np.float64) / 1e9

    assert is_sorted(timestamp)

    data = energy[1:] / np.diff(timestamp)
    print(f"Geometric mean: {np.exp(np.log(data).mean()):.2f} J")
    print(f"Sigma: {np.std(data):.2f} J")
