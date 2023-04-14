#include <rapl.hpp>

#include <cmath>
#include <vector>

#include <unistd.h>

#include <cpu.hpp>
#include <msr.hpp>

namespace {
double getEnergyUnitForPackage(int package) {
    static const auto units = []() {
        std::vector<double> units;
        for (int i = 0; i < cpu::getNPackages(); ++i) {
            const auto fd = msr::open(cpu::getLowestNumberedCpuForPackage(i));
            const auto result = msr::read(fd, MSR_RAPL_POWER_UNIT);
            units.push_back(pow(0.5, (double) ((result >> 8) & 0x1f)));
            close(fd);
        }
        return units;
    }();

    return units.at(package);
}
} // namespace

rapl::Sample rapl::sample(int package) {
    const auto fd = msr::open(cpu::getLowestNumberedCpuForPackage(package));

    const auto unit = getEnergyUnitForPackage(package);

    const auto sample = Sample{
#ifdef RAPL_MSR_PKG_SUPPORTED
        .pkg = unit * msr::read(fd, MSR_PKG_ENERGY_STATUS),
#endif
#ifdef RAPL_MSR_PP0_SUPPORTED
        .pp0 = unit * msr::read(fd, MSR_PP0_ENERGY_STATUS),
#endif
#ifdef RAPL_MSR_PP1_SUPPORTED
        .pp1 = unit * msr::read(fd, MSR_PP1_ENERGY_STATUS),
#endif
#ifdef RAPL_MSR_DRAM_SUPPORTED
        .dram = unit * msr::read(fd, MSR_DRAM_ENERGY_STATUS),
#endif
#ifdef RAPL_MSR_PSYS_SUPPORTED
        .psys = unit * msr::read(fd, MSR_PLATFORM_ENERGY_STATUS),
#endif
    };

    close(fd);

    return sample;
}

rapl::Sample rapl::operator+([[maybe_unused]] const rapl::Sample& left, [[maybe_unused]] const rapl::Sample& right) {
    return {
#ifdef RAPL_MSR_PKG_SUPPORTED
        .pkg = left.pkg + right.pkg,
#endif
#ifdef RAPL_MSR_PP0_SUPPORTED
        .pp0 = left.pp0 + right.pp0,
#endif
#ifdef RAPL_MSR_PP1_SUPPORTED
        .pp1 = left.pp1 + right.pp1,
#endif
#ifdef RAPL_MSR_DRAM_SUPPORTED
        .dram = left.dram + right.dram,
#endif
#ifdef RAPL_MSR_PSYS_SUPPORTED
        .psys = left.psys + right.psys,
#endif
    };
}

rapl::Sample rapl::operator-([[maybe_unused]] const rapl::Sample& left, [[maybe_unused]] const rapl::Sample& right) {
    return {
#ifdef RAPL_MSR_PKG_SUPPORTED
        .pkg = left.pkg - right.pkg,
#endif
#ifdef RAPL_MSR_PP0_SUPPORTED
        .pp0 = left.pp0 - right.pp0,
#endif
#ifdef RAPL_MSR_PP1_SUPPORTED
        .pp1 = left.pp1 - right.pp1,
#endif
#ifdef RAPL_MSR_DRAM_SUPPORTED
        .dram = left.dram - right.dram,
#endif
#ifdef RAPL_MSR_PSYS_SUPPORTED
        .psys = left.psys - right.psys,
#endif
    };
}
