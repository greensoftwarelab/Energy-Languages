#pragma once

#include <cerrno>
#include <cstdint>
#include <iostream>
#include <string>

#include <fcntl.h>
#include <unistd.h>

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
int open(int core) {
  const auto filename = "/dev/cpu/" + std::to_string(core) + "/msr";
  const auto fd = ::open(filename.c_str(), O_RDONLY);

  if (fd < 0) {
    if (errno == ENXIO) {
      std::cout << "No CPU " << core << std::endl;
      exit(1);
    } else if (errno == EIO) {
      std::cout << "CPU " << core << " doesn't support MSRs" << std::endl;
      exit(1);
    } else {
      std::cout << "Error opening " << filename << std::endl;
      exit(1);
    }
  }

  return fd;
}

uint64_t read(int fd, int which) {
  uint64_t data;

  if (pread(fd, &data, sizeof data, which) != sizeof data) {
    std::cout << "Error reading MSR" << std::endl;
    exit(1);
  }

  return data;
}
} // namespace msr
