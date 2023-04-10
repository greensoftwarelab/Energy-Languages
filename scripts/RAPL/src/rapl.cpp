#include <chrono>
#include <condition_variable>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <ios>
#include <iostream>
#include <mutex>
#include <ratio>
#include <thread>
#include <vector>

#include <linux/hw_breakpoint.h>
#include <linux/perf_event.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <unistd.h>

#include <glaze/glaze.hpp>

#include <cpu.hpp>
#include <msr.hpp>

struct KillableTimer {
    template <typename Rep, typename Period>
    bool wait(const std::chrono::duration<Rep, Period>& time) {
        std::unique_lock<std::mutex> lock(mutex);
        return !cv.wait_for(lock, time, [&] { return killed; });
    }

    void kill() {
        std::unique_lock<std::mutex> lock(mutex);
        killed = true;
        cv.notify_all();
    }

  private:
    std::mutex mutex;
    std::condition_variable cv;
    bool killed = false;
};

void aggregate([[maybe_unused]] msr::Sample& total, [[maybe_unused]] const msr::Sample& sample) {
#ifdef RAPL_MSR_PKG_SUPPORTED
    total.pkg += sample.pkg;
#endif
#ifdef RAPL_MSR_PP0_SUPPORTED
    total.pp0 += sample.pp0;
#endif
#ifdef RAPL_MSR_PP1_SUPPORTED
    total.pp1 += sample.pp1;
#endif
#ifdef RAPL_MSR_DRAM_SUPPORTED
    total.dram += sample.dram;
#endif
#ifdef RAPL_MSR_PSYS_SUPPORTED
    total.psys += sample.psys;
#endif
}

using Clock = std::chrono::high_resolution_clock;

struct Result {
    Clock::rep runtime;
    msr::Sample energy;
    uint64_t cycles;
};

template <>
struct glz::meta<msr::Sample> {
    using T = msr::Sample;
    // clang-format off
    [[maybe_unused]] static constexpr auto value = std::apply([](auto... args) { return glz::object(args...); }, std::tuple{
#ifdef RAPL_MSR_PKG_SUPPORTED
        "pkg", &T::pkg,
#endif
#ifdef RAPL_MSR_PP0_SUPPORTED
        "pp0", &T::pp0,
#endif
#ifdef RAPL_MSR_PP1_SUPPORTED
        "pp1", &T::pp1,
#endif
#ifdef RAPL_MSR_DRAM_SUPPORTED
        "dram", &T::dram,
#endif
#ifdef RAPL_MSR_PSYS_SUPPORTED
        "psys", &T::psys,
#endif
    });
    // clang-format on
};

template <>
struct glz::meta<Result> {
    using T = Result;
    [[maybe_unused]] static constexpr auto value
        = glz::object("runtime", &T::runtime, "energy", &T::energy, "cycles", &T::cycles);
};

int main(int argc, char* argv[]) {
    if (argc < 3) {
        std::cerr << "Usage: ./rapl <json-file> '<command> [args...]'" << std::endl;
        return 1;
    }

    struct perf_event_attr pe;
    memset(&pe, 0, sizeof(struct perf_event_attr));
    pe.type = PERF_TYPE_HARDWARE;
    pe.config = PERF_COUNT_HW_CPU_CYCLES;
    pe.size = sizeof(struct perf_event_attr);
    pe.inherit = 1;
    pe.disabled = 1;

    const auto fd = syscall(__NR_perf_event_open, &pe, 0, -1, -1, 0);
    if (fd == -1) {
        std::cerr << "perf_event_open failed" << std::endl;
        return 1;
    }
    ioctl(fd, PERF_EVENT_IOC_RESET, 0);

    Result result;
    std::vector<msr::Sample> previous;
    std::mutex previous_lock;

    KillableTimer timer;
    // Assume that previous will be populated within the first 10 seconds, which will be the case in normal situations.
    std::thread subprocess = std::thread([&] {
        for (;;) {
            using namespace std::chrono_literals;
            if (!timer.wait(10s)) {
                break;
            }

            std::lock_guard<std::mutex> guard(previous_lock);
            for (int package = 0; package < cpu::getNPackages(); ++package) {
                const auto sample = msr::sample(package);
                aggregate(result.energy, msr::delta(previous[package], sample));
                previous[package] = sample;
            }
        }
    });

    for (int package = 0; package < cpu::getNPackages(); ++package) {
        previous.emplace_back(msr::sample(package));
    }

    ioctl(fd, PERF_EVENT_IOC_ENABLE, 0);
    const auto start = Clock::now();
    const auto status = system(argv[2]);
    const auto end = Clock::now();
    ioctl(fd, PERF_EVENT_IOC_DISABLE, 0);

    std::lock_guard<std::mutex> guard(previous_lock);
    for (int package = 0; package < cpu::getNPackages(); ++package) {
        const auto sample = msr::sample(package);
        aggregate(result.energy, msr::delta(previous[package], sample));
    }

    if (read(fd, &result.cycles, sizeof(uint64_t)) != sizeof(uint64_t)) {
        std::cerr << "read failed" << std::endl;
        return 1;
    }

    if (status != 0) {
        return 1;
    }

    result.runtime = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();

    std::ofstream(argv[1], std::ios_base::app) << glz::write_json(result) << "\n";

    timer.kill();
    subprocess.join();
}
