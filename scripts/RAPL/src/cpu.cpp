#include <cpu.hpp>

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>

namespace {
const std::unordered_map<int, int>& getLowestNumberedCpuByPackageMap() {
    static const auto packages = []() {
        std::unordered_map<int, int> packages;
        for (int i = 0;; ++i) {
            std::ifstream file("/sys/devices/system/cpu/cpu" + std::to_string(i) + "/topology/physical_package_id");
            if (!file) {
                break;
            }

            int package;
            if (!(file >> package)) {
                std::cerr << "Error finding out which package CPU " << i << " belongs to" << std::endl;
                exit(EXIT_FAILURE);
            }

            packages.emplace(package, i);
        }

        return packages;
    }();

    return packages;
}
} // namespace

int cpu::getNCpus() {
    static const auto n = []() {
        std::ifstream file("/proc/cpuinfo");
        if (!file) {
            std::cerr << "Error opening /proc/cpuinfo" << std::endl;
            exit(EXIT_FAILURE);
        }

        int n = 0;
        std::string line;
        while (std::getline(file, line)) {
            if (line.starts_with("processor")) {
                ++n;
            }
        }

        return n;
    }();

    return n;
}

int cpu::getNPackages() {
    return getLowestNumberedCpuByPackageMap().size();
}

int cpu::getLowestNumberedCpuForPackage(int package) {
    return getLowestNumberedCpuByPackageMap().at(package);
}
