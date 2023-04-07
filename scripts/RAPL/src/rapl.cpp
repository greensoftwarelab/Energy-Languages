// Modernized from rapl-read.c originally grabbed from Vince Weaver's website.

#include <chrono>
#include <cmath>
#include <iostream>
#include <thread>
#include <vector>

#include <unistd.h>

#include <cpu.hpp>
#include <msr.hpp>

#define MAX_PACKAGES 16

int main(int argc, char* argv, char** envp) {
    if (cpu::model() == -1) {
        std::cout << "Unsupported CPU model.\n" << std::endl;
        return -1;
    }

    std::vector<double> energy_units(cpu::getNPackages());
    for (int j = 0; j < cpu::getNPackages(); j++) {
        const auto fd = msr::open(cpu::getCpuForPackage(j));
        const auto result = msr::read(fd, MSR_RAPL_POWER_UNIT);
        energy_units[j] = pow(0.5, (double) ((result >> 8) & 0x1f));
        close(fd);
    }

    std::vector<msr::Sample> previous;

    for (int package = 0; package < cpu::getNPackages(); ++package) {
        previous.emplace_back(msr::sample(package));
    }

    for (;;) {
        using namespace std::chrono_literals;
        std::this_thread::sleep_for(1s);

        for (int package = 0; package < cpu::getNPackages(); ++package) {
            const auto sample = msr::sample(package);
            for (int i = 0; i < 5; i++) {
                std::cout << (sample[i] - previous[package][i]) * energy_units[package] << " ";
            }
            std::cout << std::endl;
            previous[package] = sample;
        }
    }
}
