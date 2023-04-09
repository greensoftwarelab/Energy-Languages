#pragma once

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

namespace cpu {
const auto SUPPORTED = std::unordered_set<int>(
    {CPU_SANDYBRIDGE,     CPU_SANDYBRIDGE_EP,  CPU_IVYBRIDGE,       CPU_IVYBRIDGE_EP,  CPU_HASWELL,
     CPU_HASWELL_ULT,     CPU_HASWELL_GT3E,    CPU_HASWELL_EP,      CPU_BROADWELL,     CPU_BROADWELL_GT3E,
     CPU_BROADWELL_EP,    CPU_BROADWELL_DE,    CPU_SKYLAKE,         CPU_SKYLAKE_HS,    CPU_SKYLAKE_X,
     CPU_KNIGHTS_LANDING, CPU_KNIGHTS_MILL,    CPU_KABYLAKE_MOBILE, CPU_KABYLAKE,      CPU_ATOM_SILVERMONT,
     CPU_ATOM_AIRMONT,    CPU_ATOM_MERRIFIELD, CPU_ATOM_MOOREFIELD, CPU_ATOM_GOLDMONT, CPU_ATOM_GEMINI_LAKE,
     CPU_ATOM_DENVERTON,  CPU_ROCKETLAKE});

int model();
int getNPackages();
int getLowestNumberedCpuForPackage(int package);
} // namespace cpu
