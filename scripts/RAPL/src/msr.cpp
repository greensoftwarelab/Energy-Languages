#include <msr.hpp>

#include <cerrno>
#include <cstdlib>
#include <iostream>
#include <string>

#include <fcntl.h>
#include <unistd.h>

#include <cpu.hpp>

int msr::open(int core) {
    const auto filename = "/dev/cpu/" + std::to_string(core) + "/msr";
    const auto fd = ::open(filename.c_str(), O_RDONLY);

    if (fd < 0) {
        if (errno == ENXIO) {
            std::cerr << "No CPU " << core << std::endl;
            exit(1);
        } else if (errno == EIO) {
            std::cerr << "CPU " << core << " doesn't support MSRs" << std::endl;
            exit(1);
        } else {
            std::cerr << "Error opening " << filename << std::endl;
            exit(1);
        }
    }

    return fd;
}

uint64_t msr::read(int fd, int which) {
    uint64_t data;

    if (pread(fd, &data, sizeof data, which) != sizeof data) {
        std::cerr << "Error reading MSR" << std::endl;
        exit(1);
    }

    return data;
}
