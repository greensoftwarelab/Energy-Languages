#include <perf.hpp>

#include <cassert>
#include <cstring>
#include <iostream>

#include <linux/perf_event.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <unistd.h>

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
            break;
        case PERF_COUNT_HW_CACHE_L1I:
            representation = "PERF_COUNT_HW_CACHE_L1I";
            break;
        case PERF_COUNT_HW_CACHE_LL:
            representation = "PERF_COUNT_HW_CACHE_LL";
            break;
        case PERF_COUNT_HW_CACHE_DTLB:
            representation = "PERF_COUNT_HW_CACHE_DTLB";
            break;
        case PERF_COUNT_HW_CACHE_ITLB:
            representation = "PERF_COUNT_HW_CACHE_ITLB";
            break;
        case PERF_COUNT_HW_CACHE_BPU:
            representation = "PERF_COUNT_HW_CACHE_BPU";
            break;
        case PERF_COUNT_HW_CACHE_NODE:
            representation = "PERF_COUNT_HW_CACHE_NODE";
            break;
        }

        switch ((config >> 8) & 0xFF) {
        case PERF_COUNT_HW_CACHE_OP_READ:
            representation += " | PERF_COUNT_HW_CACHE_OP_READ";
            break;
        case PERF_COUNT_HW_CACHE_OP_WRITE:
            representation += " | PERF_COUNT_HW_CACHE_OP_WRITE";
            break;
        case PERF_COUNT_HW_CACHE_OP_PREFETCH:
            representation += " | PERF_COUNT_HW_CACHE_OP_PREFETCH";
            break;
        }

        switch ((config >> 16) & 0xFF) {
        case PERF_COUNT_HW_CACHE_RESULT_ACCESS:
            representation += " | PERF_COUNT_HW_CACHE_RESULT_ACCESS";
            break;
        case PERF_COUNT_HW_CACHE_RESULT_MISS:
            representation += " | PERF_COUNT_HW_CACHE_RESULT_MISS";
            break;
        }

        return representation;
    }

    return "[unknown]";
}

perf::Group::Group(const std::vector<std::pair<int, int>>& events) {
    assert(!events.empty());

    struct perf_event_attr pe;
    std::memset(&pe, 0, sizeof(struct perf_event_attr));
    pe.size = sizeof(struct perf_event_attr);
    pe.type = events[0].first;
    pe.config = events[0].second;
    pe.inherit = 1;
    pe.disabled = 1;

    const auto leader = syscall(SYS_perf_event_open, &pe, 0, -1, -1, 0);
    if (leader == -1) {
        std::cerr << "perf_event_open failed for event " << perf::toString(pe.type, pe.config) << std::endl;
        exit(EXIT_FAILURE);
    }
    if (ioctl(leader, PERF_EVENT_IOC_RESET, PERF_IOC_FLAG_GROUP)) {
        std::cerr << "ioctl(RESET) failed for event " << perf::toString(pe.type, pe.config) << std::endl;
        exit(EXIT_FAILURE);
    }
    descriptors.push_back(leader);

    for (std::size_t i = 1; i < events.size(); ++i) {
        struct perf_event_attr pe;
        std::memset(&pe, 0, sizeof(struct perf_event_attr));
        pe.size = sizeof(struct perf_event_attr);
        pe.type = events[i].first;
        pe.config = events[i].second;
        pe.inherit = 1;

        const auto fd = syscall(SYS_perf_event_open, &pe, 0, -1, leader, 0);
        if (fd == -1) {
            std::cerr << "perf_event_open failed for event " << perf::toString(pe.type, pe.config) << std::endl;
            exit(EXIT_FAILURE);
        }
        if (ioctl(fd, PERF_EVENT_IOC_RESET, PERF_IOC_FLAG_GROUP)) {
            std::cerr << "ioctl(RESET) failed for event " << perf::toString(pe.type, pe.config) << std::endl;
            exit(EXIT_FAILURE);
        }
        descriptors.push_back(fd);
    }
}

perf::Group::~Group() {
    for (const auto fd : descriptors) {
        close(fd);
    }
}

void perf::Group::enable() {
    if (ioctl(descriptors[0], PERF_EVENT_IOC_ENABLE, PERF_IOC_FLAG_GROUP)) {
        std::cerr << "ioctl(ENABLE) failed" << std::endl;
        exit(EXIT_FAILURE);
    }
    enabled = true;
}

void perf::Group::disable() {
    if (ioctl(descriptors[0], PERF_EVENT_IOC_DISABLE, PERF_IOC_FLAG_GROUP)) {
        std::cerr << "ioctl(DISABLE) failed" << std::endl;
        exit(EXIT_FAILURE);
    }
    enabled = false;
}

void perf::Group::reset() {
    for (const auto fd : descriptors) {
        if (ioctl(fd, PERF_EVENT_IOC_RESET, PERF_IOC_FLAG_GROUP)) {
            std::cerr << "ioctl(RESET) failed" << std::endl;
            exit(EXIT_FAILURE);
        }
    }
}

bool perf::Group::isEnabled() {
    return enabled;
}

std::vector<std::uint64_t> perf::Group::read() const {
    // This restriction is due to `inherit` and `PERF_FORMAT_GROUP` not being compatible.
    assert(!isEnabled());

    std::vector<std::uint64_t> values(descriptors.size());

    for (std::size_t i = 0; i < descriptors.size(); ++i) {
        if (::read(descriptors[i], &values[i], sizeof(std::uint64_t)) != sizeof(std::uint64_t)) {
            std::cerr << "read failed" << std::endl;
            exit(EXIT_FAILURE);
        }
    }

    return values;
}
