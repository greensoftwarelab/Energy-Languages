import collections
import os
import json
import sys
import matplotlib.pyplot as plt


ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_ROOT = os.path.join(ROOT, "data", "obelix96")
LANGUAGES = ["C", "C++", "Rust", "Java", "Go", "JavaScript", "TypeScript", "Python"]
STYLES = {"C": 'black', "C++": 'turquoise', "Rust": 'brown', "Java": 'red',
          "Go": 'orange', "JavaScript": 'green', "TypeScript": 'blue', "Python": 'purple'}


def get_avg(langbench_pair: list[tuple]) -> tuple:
    """Returns average time and energy pair of a set of runs."""
    result = (0, 0)
    N = len(langbench_pair)
    for time, energy in langbench_pair:
        result = result[0] + (time / N), result[1] + (energy / N)
    return result


if __name__ == "__main__":
    argc = len(sys.argv)
    view = 'default' if argc < 2 else sys.argv[1]

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
                    if line['energy']['pkg'] < 0:
                        continue
                    data[language][benchmark].append((line['runtime'], line['energy']['pkg']))

    benchmarks = sorted(list({b for l in data.values() for b in l.keys()}))
    plt.figure(figsize=(10, 7))
    if view == 'medium':
        plt.axis([0, 40000, 0, 2000])
    elif view == 'large':
        plt.axis([0, 10000, 0, 750])
    elif view == 'logscale':
        plt.xscale('log')
        plt.yscale('log')
    plt.title(f'Time v. Energy: {view} view')
    plt.xlabel('Time (ms)')
    plt.ylabel('Energy (J)')
    for language in data:
        times, energies = [], []
        for benchmark in data[language]:
            time, energy = get_avg(data[language][benchmark])
            times.append(time)
            energies.append(energy)
        plt.scatter(times, energies, color=STYLES[language], label=language)
    plt.legend(loc='upper left')
    plt.savefig(f'time_v_energy_{view}.jpg')
    plt.show()
