#pragma once

#include <msr.hpp>

namespace rapl {
struct DoubleSample {
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

    rapl::DoubleSample& operator+=([[maybe_unused]] const rapl::DoubleSample& right) {
#ifdef RAPL_MSR_PKG_SUPPORTED
        this->pkg += right.pkg;
#endif
#ifdef RAPL_MSR_PP0_SUPPORTED
        this->pp0 += right.pp0;
#endif
#ifdef RAPL_MSR_PP1_SUPPORTED
        this->pp1 += right.pp1;
#endif
#ifdef RAPL_MSR_DRAM_SUPPORTED
        this->dram += right.dram;
#endif
        return *this;
    }

    friend rapl::DoubleSample operator+(rapl::DoubleSample left, const rapl::DoubleSample& right) {
        left += right;
        return left;
    }

    rapl::DoubleSample& operator-=([[maybe_unused]] const rapl::DoubleSample& right) {
#ifdef RAPL_MSR_PKG_SUPPORTED
        this->pkg -= right.pkg;
#endif
#ifdef RAPL_MSR_PP0_SUPPORTED
        this->pp0 -= right.pp0;
#endif
#ifdef RAPL_MSR_PP1_SUPPORTED
        this->pp1 -= right.pp1;
#endif
#ifdef RAPL_MSR_DRAM_SUPPORTED
        this->dram -= right.dram;
#endif
        return *this;
    }

    friend rapl::DoubleSample operator-(rapl::DoubleSample left, const rapl::DoubleSample& right) {
        left -= right;
        return left;
    }
};

DoubleSample sample(int package);
} // namespace rapl
