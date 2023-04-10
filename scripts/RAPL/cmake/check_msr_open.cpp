#include <fcntl.h>
#include <iostream>
#include <unistd.h>

int main() {
    const auto fd = open("/dev/cpu/0/msr", O_RDONLY);
    if (fd == -1) {
        std::cout << "This actually fails" << std::endl;
        return 1;
    }
    close(fd);
    std::cout << "It worked..." << std::endl;
    return 0;
}
