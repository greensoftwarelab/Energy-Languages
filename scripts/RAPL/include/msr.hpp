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
#ifdef RAPL_MSR_PKG_SUPPORTED
    double pkg = 0;
#endif
#ifdef RAPL_MSR_PP0_SUPPORTED
    double pp0 = 0;
#endif
#ifdef RAPL_MSR_PP1_SUPPORTED
    double pp1 = 0;
#endif
#ifdef RAPL_MSR_DRAM_SUPPORTED
    double dram = 0;
#endif
#ifdef RAPL_MSR_PSYS_SUPPORTED
    double psys = 0;
#endif
};

// The returned file descriptor should be closed by the caller.
int open(int core);
int64_t read(int fd, int which);
Sample sample(int package);
Sample delta(const Sample& previous, const Sample& current);
} // namespace msr
