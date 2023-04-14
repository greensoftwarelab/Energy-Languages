#include <cstdint>
#include <fcntl.h>
#include <unistd.h>

#define MSR_PLATFORM_ENERGY_STATUS 0x64d

int main() {
    const auto fd = open("/dev/cpu/0/msr", O_RDONLY);
    if (fd == -1) {
        return 1;
    }

    uint64_t data;
    if (pread(fd, &data, sizeof(uint64_t), MSR_PLATFORM_ENERGY_STATUS) != sizeof(uint64_t)) {
        return 1;
    }
}
