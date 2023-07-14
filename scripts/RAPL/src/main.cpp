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
#include <unordered_map>
#include <vector>

#include <linux/hw_breakpoint.h>
#include <linux/perf_event.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <glaze/glaze.hpp>

#include <range/v3/all.hpp>

#include <cpu.hpp>
#include <msr.hpp>
#include <perf.hpp>
#include <rapl.hpp>

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

using Clock = std::chrono::high_resolution_clock;

struct Result {
    Clock::rep runtime;
    rapl::DoubleSample energy;
    std::unordered_map<std::string, std::uint64_t> perf_counters;
};

template <>
struct glz::meta<rapl::DoubleSample> {
    using T = rapl::DoubleSample;
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
    });
    // clang-format on
};

struct ScopeExit {
    ScopeExit(std::function<void()> f) : f(std::move(f)) {}
    ~ScopeExit() {
        f();
    }
    std::function<void()> f;
};

template <>
struct glz::meta<Result> {
    using T = Result;
    [[maybe_unused]] static constexpr auto value
        = glz::object("runtime", &T::runtime, "energy", &T::energy, "perf_counters", &T::perf_counters);
};

int main(int argc, char* argv[]) {
    if (argc < 3) {
        std::cerr << "Usage: ./rapl <json-file> <command> [args...]" << std::endl;
        return 1;
    }

    std::string command = argv[2];
    for (int i = 3; i < argc; ++i) {
        command.append(" ");
        command.append(argv[i]);
    }

    std::vector<std::pair<int, int>> events
        = {// {PERF_TYPE_HARDWARE, PERF_COUNT_HW_REF_CPU_CYCLES},
           {PERF_TYPE_HARDWARE, PERF_COUNT_HW_CPU_CYCLES},
           {PERF_TYPE_HARDWARE, PERF_COUNT_HW_INSTRUCTIONS},
           {PERF_TYPE_HW_CACHE,
            PERF_COUNT_HW_CACHE_LL | (PERF_COUNT_HW_CACHE_OP_READ << 8) | (PERF_COUNT_HW_CACHE_RESULT_ACCESS << 16)}};

    perf::Group perfEventGroup(events);

    Result result;
    std::vector<rapl::U32Sample> previous;
    std::mutex lock;

    KillableTimer timer;
    std::thread subprocess = std::thread([&] {
        for (;;) {
            using namespace std::chrono_literals;
            if (!timer.wait(1s)) {
                break;
            }

            std::lock_guard<std::mutex> guard(lock);
            for (int package = 0; package < cpu::getNPackages(); ++package) {
                const auto sample = rapl::sample(package);
                result.energy += rapl::scale(sample - previous[package], package);
                previous[package] = sample;
            }
        }
    });
    ScopeExit _([&] {
        timer.kill();
        subprocess.join();
    });

    {
        std::lock_guard<std::mutex> guard(lock);
        for (int package = 0; package < cpu::getNPackages(); ++package) {
            previous.emplace_back(rapl::sample(package));
        }
    }

    perfEventGroup.enable();
    const auto start = Clock::now();
    const auto status = std::system(command.c_str());
    const auto end = Clock::now();
    perfEventGroup.disable();

    std::lock_guard<std::mutex> guard(lock);
    for (int package = 0; package < cpu::getNPackages(); ++package) {
        const auto sample = rapl::sample(package);
        result.energy += rapl::scale(sample - previous[package], package);
    }

    const auto perf_counters = perfEventGroup.read();
    result.perf_counters = perf_counters | ranges::views::enumerate | ranges::views::transform([&](const auto& p) {
                               const auto [type, config] = events[p.first];
                               return std::make_pair(perf::toString(type, config), p.second);
                           })
                           | ranges::to<std::unordered_map<std::string, std::uint64_t>>;

    if (status != 0) {
        std::cerr << "child process failed" << std::endl;
        return 1;
    }

    result.runtime = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();

    if (!(std::ofstream(argv[1], std::ios_base::app) << glz::write_json(result) << "\n")) {
        std::cerr << "write failed" << std::endl;
        return 1;
    }
}
