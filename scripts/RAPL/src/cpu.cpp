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

int cpu::getNPackages() {
    return getLowestNumberedCpuByPackageMap().size();
}

int cpu::getLowestNumberedCpuForPackage(int package) {
    return getLowestNumberedCpuByPackageMap().at(package);
}
