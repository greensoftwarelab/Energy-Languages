#include <cpu.hpp>

#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>

namespace {
std::string trim(std::string s) {
    static const auto isNotSpace = [](auto c) { return !std::isspace(c); };
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), isNotSpace));
    s.erase(std::find_if(s.rbegin(), s.rend(), isNotSpace).base(), s.end());
    return s;
}

const std::unordered_map<int, int>& getLowestNumberedCpuByPackageMap() {
    static const auto packages = []() {
        std::unordered_map<int, int> packages;
        for (int i = 0;; ++i) {
            std::ifstream file("/sys/devices/system/cpu/cpu" + std::to_string(i) + "/topology/physical_package_id");
            if (!file) {
                break;
            }

            int package;
            file >> package;
            if (!file) {
                std::cerr << "Error finding out which package CPU " << i << " belongs to" << std::endl;
                exit(1);
            }

            packages.emplace(package, i);
        }

        return packages;
    }();

    return packages;
};
} // namespace

// Note: This may not mork in multi-CPU configurations with different models.
// Such systems are not common.
int cpu::model() {
    int model = -1;
    std::ifstream file("/proc/cpuinfo");
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) {
            continue;
        }

        const auto index = line.find(':');
        assert(index != std::string::npos);
        const auto key = trim(line.substr(0, index));
        const auto value = trim(line.substr(index + 1));

        if (key == "vendor_id") {
            if (value != "GenuineIntel") {
                std::cerr << value << " not an Intel chip" << std::endl;
                return -1;
            }
        }

        if (key == "cpu family") {
            const auto family = std::stoi(value);
            if (family != 6) {
                std::cerr << "Wrong CPU family " << family << std::endl;
                return -1;
            }
        }

        if (key == "model") {
            model = std::stoi(value);

            if (!SUPPORTED.contains(model)) {
                std::cerr << "Unsupported CPU model " << model << std::endl;
                return -1;
            }
        }
    }

    return model;
}

int cpu::getNPackages() {
    return getLowestNumberedCpuByPackageMap().size();
}

int cpu::getLowestNumberedCpuForPackage(int package) {
    return getLowestNumberedCpuByPackageMap().at(package);
}
