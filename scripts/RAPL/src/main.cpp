#include <chrono>
#include <condition_variable>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <iostream>
#include <mutex>
#include <string>
#include <thread>
#include <tuple>
#include <utility>
#include <vector>

#include <sys/resource.h>
#include <sys/time.h>

#include <glaze/core/common.hpp>
#include <glaze/json/write.hpp>
namespace glz {
template <class T>
struct meta;
}

#include <cpu.hpp>
#include <rapl.hpp>

using Clock = std::chrono::high_resolution_clock;

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

struct ScopeExit {
    ScopeExit(std::function<void()> f) : f(std::move(f)) {}
    ~ScopeExit() {
        f();
    }
    std::function<void()> f;
};

struct timeval& operator-=(struct timeval& left, const struct timeval& right) {
    left.tv_sec -= right.tv_sec;
    left.tv_usec -= right.tv_usec;
    if (left.tv_usec < 0) {
        left.tv_sec -= 1;
        left.tv_usec += 1000000;
    }
    return left;
}

struct rusage& operator-=(struct rusage& left, const struct rusage& right) {
    left.ru_utime -= right.ru_utime;
    left.ru_stime -= right.ru_stime;
    left.ru_maxrss -= right.ru_maxrss;
    left.ru_ixrss -= right.ru_ixrss;
    left.ru_idrss -= right.ru_idrss;
    left.ru_isrss -= right.ru_isrss;
    left.ru_minflt -= right.ru_minflt;
    left.ru_majflt -= right.ru_majflt;
    left.ru_nswap -= right.ru_nswap;
    left.ru_inblock -= right.ru_inblock;
    left.ru_oublock -= right.ru_oublock;
    left.ru_msgsnd -= right.ru_msgsnd;
    left.ru_msgrcv -= right.ru_msgrcv;
    left.ru_nsignals -= right.ru_nsignals;
    left.ru_nvcsw -= right.ru_nvcsw;
    left.ru_nivcsw -= right.ru_nivcsw;

    return left;
}

struct timeval operator-(struct timeval left, const struct timeval& right) {
    left -= right;
    return left;
}

struct rusage operator-(struct rusage left, const struct rusage& right) {
    left -= right;
    return left;
}

struct Result {
    Clock::rep runtime;
    rapl::DoubleSample energy;
    struct rusage rusage;
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

template <>
struct glz::meta<struct timeval> {
    using T = struct timeval;
    [[maybe_unused]] static constexpr auto value = glz::object("tv_sec", &T::tv_sec, "tv_usec", &T::tv_usec);
};

template <>
struct glz::meta<struct rusage> {
    using T = struct rusage;
    // clang-format off
    [[maybe_unused]] static constexpr auto value = glz::object(
            "ru_utime", &T::ru_utime,
            "ru_stime", &T::ru_stime,
            "ru_maxrss", &T::ru_maxrss,
            "ru_ixrss", &T::ru_ixrss,
            "ru_idrss", &T::ru_idrss,
            "ru_isrss", &T::ru_isrss,
            "ru_minflt", &T::ru_minflt,
            "ru_majflt", &T::ru_majflt,
            "ru_nswap", &T::ru_nswap,
            "ru_inblock", &T::ru_inblock,
            "ru_oublock", &T::ru_oublock,
            "ru_msgsnd", &T::ru_msgsnd,
            "ru_msgrcv", &T::ru_msgrcv,
            "ru_nsignals", &T::ru_nsignals,
            "ru_nvcsw", &T::ru_nvcsw,
            "ru_nivcsw", &T::ru_nivcsw
    );
    // clang-format on
};

template <>
struct glz::meta<Result> {
    using T = Result;
    [[maybe_unused]] static constexpr auto value
        = glz::object("runtime", &T::runtime, "energy", &T::energy, "rusage", &T::rusage);
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

    Result result;
    std::vector<rapl::U32Sample> previous;
    std::mutex lock;

    {
        std::lock_guard<std::mutex> guard(lock);
        for (int package = 0; package < cpu::getNPackages(); ++package) {
            previous.emplace_back(rapl::sample(package));
        }
    }

    KillableTimer timer;
    std::thread subprocess = std::thread([&] {
        for (;;) {
            if (!timer.wait(std::chrono::seconds(1))) {
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

    struct rusage start_usage;
    struct rusage end_usage;

    if (getrusage(RUSAGE_CHILDREN, &start_usage) != 0) {
        std::cerr << "getrusage failed" << std::endl;
        return 1;
    }
    const auto start = Clock::now();
    const auto status = std::system(command.c_str());
    const auto end = Clock::now();
    if (getrusage(RUSAGE_CHILDREN, &end_usage) != 0) {
        std::cerr << "getrusage failed" << std::endl;
        return 1;
    }

    std::lock_guard<std::mutex> guard(lock);
    for (int package = 0; package < cpu::getNPackages(); ++package) {
        const auto sample = rapl::sample(package);
        result.energy += rapl::scale(sample - previous[package], package);
    }

    if (status != 0) {
        std::cerr << "child process failed" << std::endl;
        return 1;
    }

    result.runtime = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
    result.rusage = end_usage - start_usage;

    if (!(std::ofstream(argv[1], std::ios_base::app) << glz::write_json(result) << "\n")) {
        std::cerr << "write failed" << std::endl;
        return 1;
    }
}
