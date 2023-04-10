#pragma once

namespace cpu {
int getNCpus();
int getNPackages();
int getLowestNumberedCpuForPackage(int package);
} // namespace cpu
