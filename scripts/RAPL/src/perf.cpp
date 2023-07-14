#include <perf.hpp>

#include <linux/perf_event.h>

std::string perf::toString(int type, int config) {
    if (type == PERF_TYPE_HARDWARE) {
        switch (config) {
        case PERF_COUNT_HW_CPU_CYCLES:
            return "PERF_COUNT_HW_CPU_CYCLES";
        case PERF_COUNT_HW_INSTRUCTIONS:
            return "PERF_COUNT_HW_INSTRUCTIONS";
        case PERF_COUNT_HW_CACHE_REFERENCES:
            return "PERF_COUNT_HW_CACHE_REFERENCES";
        case PERF_COUNT_HW_CACHE_MISSES:
            return "PERF_COUNT_HW_CACHE_MISSES";
        case PERF_COUNT_HW_BRANCH_INSTRUCTIONS:
            return "PERF_COUNT_HW_BRANCH_INSTRUCTIONS";
        case PERF_COUNT_HW_BRANCH_MISSES:
            return "PERF_COUNT_HW_BRANCH_MISSES";
        case PERF_COUNT_HW_BUS_CYCLES:
            return "PERF_COUNT_HW_BUS_CYCLES";
        case PERF_COUNT_HW_STALLED_CYCLES_FRONTEND:
            return "PERF_COUNT_HW_STALLED_CYCLES_FRONTEND";
        case PERF_COUNT_HW_STALLED_CYCLES_BACKEND:
            return "PERF_COUNT_HW_STALLED_CYCLES_BACKEND";
        case PERF_COUNT_HW_REF_CPU_CYCLES:
            return "PERF_COUNT_HW_REF_CPU_CYCLES";
        }
    } else if (type == PERF_TYPE_SOFTWARE) {
        switch (config) {
        case PERF_COUNT_SW_CPU_CLOCK:
            return "PERF_COUNT_SW_CPU_CLOCK";
        case PERF_COUNT_SW_TASK_CLOCK:
            return "PERF_COUNT_SW_TASK_CLOCK";
        case PERF_COUNT_SW_PAGE_FAULTS:
            return "PERF_COUNT_SW_PAGE_FAULTS";
        case PERF_COUNT_SW_CONTEXT_SWITCHES:
            return "PERF_COUNT_SW_CONTEXT_SWITCHES";
        case PERF_COUNT_SW_CPU_MIGRATIONS:
            return "PERF_COUNT_SW_CPU_MIGRATIONS";
        case PERF_COUNT_SW_PAGE_FAULTS_MIN:
            return "PERF_COUNT_SW_PAGE_FAULTS_MIN";
        case PERF_COUNT_SW_PAGE_FAULTS_MAJ:
            return "PERF_COUNT_SW_PAGE_FAULTS_MAJ";
        case PERF_COUNT_SW_ALIGNMENT_FAULTS:
            return "PERF_COUNT_SW_ALIGNMENT_FAULTS";
        case PERF_COUNT_SW_EMULATION_FAULTS:
            return "PERF_COUNT_SW_EMULATION_FAULTS";
        case PERF_COUNT_SW_DUMMY:
            return "PERF_COUNT_SW_DUMMY";
        case PERF_COUNT_SW_BPF_OUTPUT:
            return "PERF_COUNT_SW_BPF_OUTPUT";
        case PERF_COUNT_SW_CGROUP_SWITCHES:
            return "PERF_COUNT_SW_CGROUP_SWITCHES";
        }
    } else if (type == PERF_TYPE_HW_CACHE) {
        std::string representation;

        switch (config & 0xFF) {
        case PERF_COUNT_HW_CACHE_L1D:
            representation = "PERF_COUNT_HW_CACHE_L1D";
        case PERF_COUNT_HW_CACHE_L1I:
            representation = "PERF_COUNT_HW_CACHE_L1I";
        case PERF_COUNT_HW_CACHE_LL:
            representation = "PERF_COUNT_HW_CACHE_LL";
        case PERF_COUNT_HW_CACHE_DTLB:
            representation = "PERF_COUNT_HW_CACHE_DTLB";
        case PERF_COUNT_HW_CACHE_ITLB:
            representation = "PERF_COUNT_HW_CACHE_ITLB";
        case PERF_COUNT_HW_CACHE_BPU:
            representation = "PERF_COUNT_HW_CACHE_BPU";
        case PERF_COUNT_HW_CACHE_NODE:
            representation = "PERF_COUNT_HW_CACHE_NODE";
        }

        switch ((config >> 8) & 0xFF) {
        case PERF_COUNT_HW_CACHE_OP_READ:
            representation += " | PERF_COUNT_HW_CACHE_OP_READ";
        case PERF_COUNT_HW_CACHE_OP_WRITE:
            representation += " | PERF_COUNT_HW_CACHE_OP_WRITE";
        case PERF_COUNT_HW_CACHE_OP_PREFETCH:
            representation += " | PERF_COUNT_HW_CACHE_OP_PREFETCH";
        }

        switch ((config >> 16) & 0xFF) {
        case PERF_COUNT_HW_CACHE_RESULT_ACCESS:
            representation += " | PERF_COUNT_HW_CACHE_RESULT_ACCESS";
        case PERF_COUNT_HW_CACHE_RESULT_MISS:
            representation += " | PERF_COUNT_HW_CACHE_RESULT_MISS";
        }

        return representation;
    }

    return "[unknown]";
}
