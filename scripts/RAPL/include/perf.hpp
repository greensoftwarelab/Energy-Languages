#pragma once

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

namespace perf {
std::string toString(int type, int config);

struct Group {
    Group(const std::vector<std::pair<int, int>>& events);
    ~Group();

    Group(const Group&) = delete;
    Group& operator=(const Group&) = delete;

    Group(Group&&) = default;
    Group& operator=(Group&&) = default;

    void enable();
    void disable();
    void reset();

    bool isEnabled();

    std::vector<std::uint64_t> read() const;

  private:
    std::vector<int> descriptors;
    bool enabled = false;
};
} // namespace perf
