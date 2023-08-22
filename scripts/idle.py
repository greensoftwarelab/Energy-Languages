import argparse

import numpy as np
import pandas


def is_sorted(l):
    return all(l[i] <= l[i + 1] for i in range(len(l) - 1))


def main(args):
    data = pandas.read_csv(
        args.path,
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


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--path", type=str, required=True)
    main(parser.parse_args())
