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
    total.pkg += sample.pkg;
    total.pp0 += sample.pp0;
    total.pp1 += sample.pp1;
    total.dram += sample.dram;
    total.psys += sample.psys;
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
        const auto fd = msr::open(cpu::getLowestNumberedCpuForPackage(j));
        const auto result = msr::read(fd, MSR_RAPL_POWER_UNIT);
        energy_units[j] = pow(0.5, (double) ((result >> 8) & 0x1f));
        close(fd);
    }

    // TODO: This should be doubles. Otherwise there is no reason to ever sample at fixed time, this will overflow just
    // the same.
    msr::Sample total = {0, 0, 0, 0, 0};
    std::vector<msr::Sample> previous;
    std::mutex previous_lock;

    for (int package = 0; package < cpu::getNPackages(); ++package) {
        previous.emplace_back(msr::sample(package));
    }

    std::thread subprocess = std::thread([&]() {
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

        for (int package = 0; package < cpu::getNPackages(); ++package) {
            std::cerr << "Package " << package << ":" << std::endl;
            std::cerr << "\tPKG  Energy: " << energy_units[package] * total.pkg << "J" << std::endl;
            std::cerr << "\tPP0  Energy: " << energy_units[package] * total.pp0 << "J" << std::endl;
            std::cerr << "\tPP1  Energy: " << energy_units[package] * total.pp1 << "J" << std::endl;
            std::cerr << "\tDRAM Energy: " << energy_units[package] * total.dram << "J" << std::endl;
            std::cerr << "\tPSYS Energy: " << energy_units[package] * total.psys << "J" << std::endl;
            std::cout << std::endl;
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
