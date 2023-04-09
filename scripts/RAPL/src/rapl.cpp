// Modernized from rapl-read.c originally grabbed from Vince Weaver's website.

#include <cassert>
#include <cerrno>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <mutex>
#include <thread>
#include <vector>

#include <unistd.h>

#include <cpu.hpp>
#include <msr.hpp>

void aggregate(msr::Sample& total, const msr::Sample& sample) {
    assert(total.size() == sample.size());
    for (size_t i = 0; i < sample.size(); ++i) {
        total[i] += sample[i];
    }
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: ./rapl <command> [args...]" << std::endl;
        exit(1);
    }

    if (cpu::model() == -1) {
        std::cerr << "Unsupported CPU model.\n" << std::endl;
        return -1;
    }

    std::vector<double> energy_units(cpu::getNPackages());
    for (int j = 0; j < cpu::getNPackages(); j++) {
        const auto fd = msr::open(cpu::getCpuForPackage(j));
        const auto result = msr::read(fd, MSR_RAPL_POWER_UNIT);
        energy_units[j] = pow(0.5, (double) ((result >> 8) & 0x1f));
        close(fd);
    }

    msr::Sample total = {0, 0, 0, 0, 0};
    std::vector<msr::Sample> previous;
    std::mutex previous_lock;

    for (int package = 0; package < cpu::getNPackages(); ++package) {
        previous.emplace_back(msr::sample(package));
    }

    std::thread subprocess = std::thread([&argv]() {
        if (system(argv[1]) == -1) {
            std::cerr << "Failed to execute command: " << strerror(errno) << std::endl;
            exit(1);
        }
    });
    std::thread joiner = std::thread([&]() {
        subprocess.join();
        std::lock_guard<std::mutex> guard(previous_lock);
        for (int package = 0; package < cpu::getNPackages(); ++package) {
            const auto sample = msr::sample(package);
            aggregate(total, msr::delta(previous[package], sample));
            previous[package] = sample;
        }

        std::cerr << "Subprocess exited." << std::endl;
        for (size_t i = 0; i < total.size(); ++i) {
            std::cerr << "Energy consumed: " << energy_units[0] * total[i] << " Joules" << std::endl;
        }
        exit(0);
    });

    for (;;) {
        using namespace std::chrono_literals;
        std::this_thread::sleep_for(10s);

        std::lock_guard<std::mutex> guard(previous_lock);
        for (int package = 0; package < cpu::getNPackages(); ++package) {
            const auto sample = msr::sample(package);
            aggregate(total, msr::delta(previous[package], sample));
            previous[package] = sample;
        }
    }
}
