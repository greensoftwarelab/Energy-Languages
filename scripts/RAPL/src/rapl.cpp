#include <chrono>
#include <condition_variable>
#include <cstdlib>
#include <iostream>
#include <mutex>
#include <ratio>
#include <thread>
#include <vector>

#include <cpu.hpp>
#include <msr.hpp>

struct KillableWait {
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

void aggregate(msr::Sample& total, const msr::Sample& sample) {
    total.pkg += sample.pkg;
    total.pp0 += sample.pp0;
    total.pp1 += sample.pp1;
    total.dram += sample.dram;
    total.psys += sample.psys;
}

using Clock = std::chrono::high_resolution_clock;

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: ./rapl '<command> [args...]'" << std::endl;
        return 1;
    }

    msr::Sample total;
    std::vector<msr::Sample> previous;
    std::mutex previous_lock;

    for (int package = 0; package < cpu::getNPackages(); ++package) {
        previous.emplace_back(msr::sample(package));
    }

    KillableWait timer;
    std::thread subprocess = std::thread([&]() {
        for (;;) {
            using namespace std::chrono_literals;
            if (!timer.wait(10s)) {
                break;
            }

            std::lock_guard<std::mutex> guard(previous_lock);
            for (int package = 0; package < cpu::getNPackages(); ++package) {
                const auto sample = msr::sample(package);
                aggregate(total, msr::delta(previous[package], sample));
                previous[package] = sample;
            }
        }
    });

    const auto start = Clock::now();
    const auto status = system(argv[1]);
    const auto end = Clock::now();

    if (status != 0) {
        return 1;
    }

    std::lock_guard<std::mutex> guard(previous_lock);
    for (int package = 0; package < cpu::getNPackages(); ++package) {
        const auto sample = msr::sample(package);
        aggregate(total, msr::delta(previous[package], sample));
    }

    const auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
    std::cerr << "Time elapsed: " << elapsed << "ms" << std::endl;
    std::cerr << "PKG  Energy : " << total.pkg << "J" << std::endl;
    std::cerr << "PP0  Energy : " << total.pp0 << "J" << std::endl;
    std::cerr << "PP1  Energy : " << total.pp1 << "J" << std::endl;
    std::cerr << "DRAM Energy : " << total.dram << "J" << std::endl;
    std::cerr << "PSYS Energy : " << total.psys << "J" << std::endl;

    timer.kill();
    subprocess.join();
}
