#pragma once

#include <msr.hpp>

namespace rapl {
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

Sample sample(int package);
Sample delta(const Sample& previous, const Sample& current);
} // namespace rapl
