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

rapl::U32Sample rapl::sample(int package) {
    [[maybe_unused]] const auto fd = msr::open(cpu::getLowestNumberedCpuForPackage(package));

    const auto sample = U32Sample{
#ifdef RAPL_MSR_PKG_SUPPORTED
        .pkg = static_cast<uint32_t>(msr::read(fd, MSR_PKG_ENERGY_STATUS)),
#endif
#ifdef RAPL_MSR_PP0_SUPPORTED
        .pp0 = static_cast<uint32_t>(msr::read(fd, MSR_PP0_ENERGY_STATUS)),
#endif
#ifdef RAPL_MSR_PP1_SUPPORTED
        .pp1 = static_cast<uint32_t>(msr::read(fd, MSR_PP1_ENERGY_STATUS)),
#endif
#ifdef RAPL_MSR_DRAM_SUPPORTED
        .dram = static_cast<uint32_t>(msr::read(fd, MSR_DRAM_ENERGY_STATUS)),
#endif
    };

    close(fd);

    return sample;
}

rapl::DoubleSample rapl::scale(rapl::U32Sample sample, int package) {
    [[maybe_unused]] const auto unit = getEnergyUnitForPackage(package);

    return rapl::DoubleSample{
#ifdef RAPL_MSR_PKG_SUPPORTED
        .pkg = unit * sample.pkg,
#endif
#ifdef RAPL_MSR_PP0_SUPPORTED
        .pp0 = unit * sample.pp0,
#endif
#ifdef RAPL_MSR_PP1_SUPPORTED
        .pp1 = unit * sample.pp1,
#endif
#ifdef RAPL_MSR_DRAM_SUPPORTED
        .dram = unit * sample.dram,
#endif
    };
}
