#include <chrono>
#include <fstream>
#include <iostream>
#include <thread>
#include <vector>

#include <cpu.hpp>
#include <rapl.hpp>

using Clock = std::chrono::high_resolution_clock;

#ifndef RAPL_MSR_PKG_SUPPORTED
#error "This tool requires MSR PKG domain support"
#endif

#define N 600 // ~ 10 minutes

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: ./idle <csv-file>" << std::endl;
        return 1;
    }

    std::ofstream csv(argv[1]);
    if (!csv) {
        std::cerr << "failed to open " << argv[1] << std::endl;
        return 1;
    }

    const auto baseline = Clock::now();
    std::vector<rapl::U32Sample> previous;
    for (int package = 0; package < cpu::getNPackages(); ++package) {
        previous.emplace_back(rapl::sample(package));
    }

    std::vector<rapl::U32Sample> sample;
    for (int i = 0; i < N; ++i) {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        const auto timestamp = std::chrono::duration_cast<std::chrono::nanoseconds>(Clock::now() - baseline).count();

        rapl::DoubleSample total;
        for (int package = 0; package < cpu::getNPackages(); ++package) {
            const auto sample = rapl::sample(package);
            total += rapl::scale(sample - previous[package], package);
            previous[package] = sample;
        }

        csv << timestamp << "," << total.pkg << std::endl;
    }
}
