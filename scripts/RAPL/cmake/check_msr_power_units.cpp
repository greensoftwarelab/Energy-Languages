#include <cstdint>
#include <fcntl.h>
#include <unistd.h>

#define MSR_RAPL_POWER_UNIT 0x606

int main() {
    const auto fd = open("/dev/cpu/0/msr", O_RDONLY);
    if (fd == -1) {
        return 1;
    }

    int64_t data;
    if (pread(fd, &data, sizeof(int64_t), MSR_RAPL_POWER_UNIT) != sizeof(int64_t)) {
        return 1;
    }

    close(fd);
}
