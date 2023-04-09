#pragma once

#include <cstdint>

#define MSR_RAPL_POWER_UNIT 0x606
#define MSR_PKG_ENERGY_STATUS 0x611
#define MSR_PP0_ENERGY_STATUS 0x639
#define MSR_PP1_ENERGY_STATUS 0x641
#define MSR_DRAM_ENERGY_STATUS 0x619
#define MSR_PLATFORM_ENERGY_STATUS 0x64d

namespace msr {
struct Sample {
    int64_t pkg;
    int64_t pp0;
    int64_t pp1;
    int64_t dram;
    int64_t psys;
};

// The returned file descriptor should be closed by the caller.
int open(int core);
int64_t read(int fd, int which);
Sample sample(int package);
Sample delta(const Sample& previous, const Sample& current);
} // namespace msr
