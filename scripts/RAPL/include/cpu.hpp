#pragma once

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <unordered_set>

#define CPU_SANDYBRIDGE 42
#define CPU_SANDYBRIDGE_EP 45
#define CPU_IVYBRIDGE 58
#define CPU_IVYBRIDGE_EP 62
#define CPU_HASWELL 60
#define CPU_HASWELL_ULT 69
#define CPU_HASWELL_GT3E 70
#define CPU_HASWELL_EP 63
#define CPU_BROADWELL 61
#define CPU_BROADWELL_GT3E 71
#define CPU_BROADWELL_EP 79
#define CPU_BROADWELL_DE 86
#define CPU_SKYLAKE 78
#define CPU_SKYLAKE_HS 94
#define CPU_SKYLAKE_X 85
#define CPU_KNIGHTS_LANDING 87
#define CPU_KNIGHTS_MILL 133
#define CPU_KABYLAKE_MOBILE 142
#define CPU_KABYLAKE 158
#define CPU_ATOM_SILVERMONT 55
#define CPU_ATOM_AIRMONT 76
#define CPU_ATOM_MERRIFIELD 74
#define CPU_ATOM_MOOREFIELD 90
#define CPU_ATOM_GOLDMONT 92
#define CPU_ATOM_GEMINI_LAKE 122
#define CPU_ATOM_DENVERTON 95
#define CPU_ROCKETLAKE 140

namespace {
std::string trim(std::string s) {
  static const auto isNotSpace = [](auto c) { return !std::isspace(c); };
  s.erase(s.begin(), std::find_if(s.begin(), s.end(), isNotSpace));
  s.erase(std::find_if(s.rbegin(), s.rend(), isNotSpace).base(), s.end());
  return s;
}
} // namespace

namespace cpu {
static const auto SUPPORTED = std::unordered_set<uint8_t>(
    {CPU_SANDYBRIDGE,      CPU_SANDYBRIDGE_EP,  CPU_IVYBRIDGE,
     CPU_IVYBRIDGE_EP,     CPU_HASWELL,         CPU_HASWELL_ULT,
     CPU_HASWELL_GT3E,     CPU_HASWELL_EP,      CPU_BROADWELL,
     CPU_BROADWELL_GT3E,   CPU_BROADWELL_EP,    CPU_BROADWELL_DE,
     CPU_SKYLAKE,          CPU_SKYLAKE_HS,      CPU_SKYLAKE_X,
     CPU_KNIGHTS_LANDING,  CPU_KNIGHTS_MILL,    CPU_KABYLAKE_MOBILE,
     CPU_KABYLAKE,         CPU_ATOM_SILVERMONT, CPU_ATOM_AIRMONT,
     CPU_ATOM_MERRIFIELD,  CPU_ATOM_MOOREFIELD, CPU_ATOM_GOLDMONT,
     CPU_ATOM_GEMINI_LAKE, CPU_ATOM_DENVERTON,  CPU_ROCKETLAKE});

uint8_t model() {
  uint8_t model = -1;
  std::ifstream file("/proc/cpuinfo");
  std::string line;

  while (std::getline(file, line)) {
    if (line.empty()) {
      continue;
    }

    const auto index = line.find(':');
    assert(index != std::string::npos);
    const auto key = trim(line.substr(0, index));
    const auto value = trim(line.substr(index + 1));

    if (key == "vendor_id") {
      if (value != "GenuineIntel") {
        std::cout << value << " not an Intel chip" << std::endl;
        return -1;
      }
    }

    if (key == "cpu family") {
      const auto family = std::stoi(value);
      if (family != 6) {
        std::cout << "Wrong CPU family " << family << std::endl;
        return -1;
      }
    }

    if (key == "model") {
      model = std::stoi(value);

      if (!SUPPORTED.contains(model)) {
        std::cout << "Unsupported CPU model " << model << std::endl;
        return -1;
      }
    }
  }

  return model;
}
} // namespace cpu
