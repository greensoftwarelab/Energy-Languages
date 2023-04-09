#pragma once

#include <array>
#include <cerrno>
#include <cstdint>
#include <iostream>
#include <string>

#include <fcntl.h>
#include <unistd.h>

#include <cpu.hpp>

#define MSR_RAPL_POWER_UNIT 0x606
#define MSR_PKG_ENERGY_STATUS 0x611
#define MSR_PP0_ENERGY_STATUS 0x639
#define MSR_PP1_ENERGY_STATUS 0x641
#define MSR_DRAM_ENERGY_STATUS 0x619
#define MSR_PLATFORM_ENERGY_STATUS 0x64d

namespace msr {
// The returned file descriptor should be closed by the caller.
int open(int core) {
    const auto filename = "/dev/cpu/" + std::to_string(core) + "/msr";
    const auto fd = ::open(filename.c_str(), O_RDONLY);

    if (fd < 0) {
        if (errno == ENXIO) {
            std::cerr << "No CPU " << core << std::endl;
            exit(1);
        } else if (errno == EIO) {
            std::cerr << "CPU " << core << " doesn't support MSRs" << std::endl;
            exit(1);
        } else {
            std::cerr << "Error opening " << filename << std::endl;
            exit(1);
        }
    }

    return fd;
}

int64_t read(int fd, int which) {
    int64_t data;

    if (pread(fd, &data, sizeof data, which) != sizeof data) {
        std::cerr << "Error reading MSR" << std::endl;
        exit(1);
    }

    return data;
}

struct Sample {
    int64_t pkg;
    int64_t pp0;
    int64_t pp1;
    int64_t dram;
    int64_t psys;
};

// Contains sample values for the package, PP0, PP1, DRAM, and PSYS domains.
// The samples should be multiplied by the appropriate energy unit.
Sample sample(int package) {
    const auto fd = msr::open(cpu::getCpuForPackage(package));

    Sample sample{.pkg = msr::read(fd, MSR_PKG_ENERGY_STATUS),
                  .pp0 = msr::read(fd, MSR_PP0_ENERGY_STATUS),
                  .pp1 = msr::read(fd, MSR_PP1_ENERGY_STATUS),
                  .dram = msr::read(fd, MSR_DRAM_ENERGY_STATUS),
                  .psys = msr::read(fd, MSR_PLATFORM_ENERGY_STATUS)};

    close(fd);

    return sample;
}

Sample delta(const Sample& previous, const Sample& current) {
    return {.pkg = current.pkg - previous.pkg,
            .pp0 = current.pp0 - previous.pp0,
            .pp1 = current.pp1 - previous.pp1,
            .dram = current.dram - previous.dram,
            .psys = current.psys - previous.psys};
}
} // namespace msr
