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

/* Package RAPL Domain */
#define MSR_PKG_RAPL_POWER_LIMIT 0x610
#define MSR_PKG_ENERGY_STATUS 0x611
#define MSR_PKG_PERF_STATUS 0x613
#define MSR_PKG_POWER_INFO 0x614

/* PP0 RAPL Domain */
#define MSR_PP0_POWER_LIMIT 0x638
#define MSR_PP0_ENERGY_STATUS 0x639
#define MSR_PP0_POLICY 0x63A
#define MSR_PP0_PERF_STATUS 0x63B

/* PP1 RAPL Domain, may reflect to uncore devices */
#define MSR_PP1_POWER_LIMIT 0x640
#define MSR_PP1_ENERGY_STATUS 0x641
#define MSR_PP1_POLICY 0x642

/* DRAM RAPL Domain */
#define MSR_DRAM_POWER_LIMIT 0x618
#define MSR_DRAM_ENERGY_STATUS 0x619
#define MSR_DRAM_PERF_STATUS 0x61B
#define MSR_DRAM_POWER_INFO 0x61C

/* PSYS RAPL Domain */
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

using Sample = std::array<int64_t, 5>;

// Contains sample values for the package, PP0, PP1, DRAM, and PSYS domains.
// The samples should be multiplied by the appropriate energy unit.
Sample sample(int package) {
    const auto fd = msr::open(cpu::getCpuForPackage(package));

    Sample sample;
    sample[0] = msr::read(fd, MSR_PKG_ENERGY_STATUS);
    sample[1] = msr::read(fd, MSR_PP0_ENERGY_STATUS);
    sample[2] = msr::read(fd, MSR_PP1_ENERGY_STATUS);
    sample[3] = msr::read(fd, MSR_DRAM_ENERGY_STATUS);
    sample[4] = msr::read(fd, MSR_PLATFORM_ENERGY_STATUS);

    close(fd);

    return sample;
}

Sample delta(const Sample& previous, const Sample& current) {
    Sample delta;
    for (size_t i = 0; i < delta.size(); ++i) {
        delta[i] = current[i] - previous[i];
    }
    return delta;
}
} // namespace msr
