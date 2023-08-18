#pragma once

#include <cstdint>

#define MSR_RAPL_POWER_UNIT 0x606
#define MSR_PKG_ENERGY_STATUS 0x611
#define MSR_PP0_ENERGY_STATUS 0x639
#define MSR_PP1_ENERGY_STATUS 0x641
#define MSR_DRAM_ENERGY_STATUS 0x619

namespace msr {
int open(int core);
std::uint64_t read(int fd, int which);
} // namespace msr
