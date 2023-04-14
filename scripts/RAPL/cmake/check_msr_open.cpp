#include <fcntl.h>
#include <unistd.h>

int main() {
    const auto fd = open("/dev/cpu/0/msr", O_RDONLY);
    if (fd == -1) {
        return 1;
    }
}
