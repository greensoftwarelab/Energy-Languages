// Modernized from rapl-read.c originally grabbed from Vince Weaver's website.

#include <fcntl.h>
#include <inttypes.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sstream>
#include <string>

#include <cpu.hpp>

#define MSR_RAPL_POWER_UNIT 0x606

/*
 * Platform specific RAPL Domains.
 * Note that PP1 RAPL Domain is supported on 062A only
 * And DRAM RAPL Domain is supported on 062D only
 */
/* Package RAPL Domain */
#define MSR_PKG_RAPL_POWER_LIMIT 0x610
#define MSR_PKG_ENERGY_STATUS 0x611
#define MSR_PKG_PERF_STATUS 0x613
#define MSR_PKG_POWER_INFO 0x614

/* PP0 RAPL Domain */
#define MSR_PP0_POWER_LIMIT 0x638
#define MSR_PP0_ENERGY_STATUS 0x639
#define MSR_PP0_POLICY 0x63A
#define MSR_PP0_PERF_STATUS 0x63B

/* PP1 RAPL Domain, may reflect to uncore devices */
#define MSR_PP1_POWER_LIMIT 0x640
#define MSR_PP1_ENERGY_STATUS 0x641
#define MSR_PP1_POLICY 0x642

/* DRAM RAPL Domain */
#define MSR_DRAM_POWER_LIMIT 0x618
#define MSR_DRAM_ENERGY_STATUS 0x619
#define MSR_DRAM_PERF_STATUS 0x61B
#define MSR_DRAM_POWER_INFO 0x61C

/* PSYS RAPL Domain */
#define MSR_PLATFORM_ENERGY_STATUS 0x64d

/* RAPL UNIT BITMASK */
#define POWER_UNIT_OFFSET 0
#define POWER_UNIT_MASK 0x0F

#define ENERGY_UNIT_OFFSET 0x08
#define ENERGY_UNIT_MASK 0x1F00

#define TIME_UNIT_OFFSET 0x10
#define TIME_UNIT_MASK 0xF000

static int open_msr(int core) {
  char msr_filename[BUFSIZ];
  int fd;

  sprintf(msr_filename, "/dev/cpu/%d/msr", core);
  fd = open(msr_filename, O_RDONLY);
  if (fd < 0) {
    if (errno == ENXIO) {
      fprintf(stderr, "rdmsr: No CPU %d\n", core);
      exit(1);
    } else if (errno == EIO) {
      fprintf(stderr, "rdmsr: CPU %d doesn't support MSRs\n", core);
      exit(1);
    } else {
      perror("rdmsr:open");
      fprintf(stderr, "Trying to open %s\n", msr_filename);
      exit(1);
    }
  }

  return fd;
}

static long long read_msr(int fd, int which) {
  uint64_t data;

  if (pread(fd, &data, sizeof data, which) != sizeof data) {
    perror("rdmsr:pread");
    exit(1);
  }

  return (long long)data;
}

#define MAX_CPUS 1024
#define MAX_PACKAGES 16

static int total_cores = 0, total_packages = 0;
static int package_map[MAX_PACKAGES];

static int detect_packages(void) {
  char filename[BUFSIZ];
  FILE *fff;
  int package;
  int i;

  for (i = 0; i < MAX_PACKAGES; i++)
    package_map[i] = -1;

  for (i = 0; i < MAX_CPUS; i++) {
    sprintf(filename,
            "/sys/devices/system/cpu/cpu%d/topology/physical_package_id", i);
    fff = fopen(filename, "r");
    if (fff == NULL)
      break;
    if (fscanf(fff, "%d", &package) != 1) {
      printf("Error reading %s\n", filename);
      exit(1);
    }
    fclose(fff);

    if (package_map[package] == -1) {
      total_packages++;
      package_map[package] = i;
    }
  }

  total_cores = i;

  return 0;
}

static int rapl_msr(int core, int cpu_model) {
  int fd;
  long long result;
  double power_units, time_units;
  double cpu_energy_units[MAX_PACKAGES], dram_energy_units[MAX_PACKAGES];
  double package_before[MAX_PACKAGES], package_after[MAX_PACKAGES];
  double pp0_before[MAX_PACKAGES], pp0_after[MAX_PACKAGES];
  double pp1_before[MAX_PACKAGES], pp1_after[MAX_PACKAGES];
  double dram_before[MAX_PACKAGES], dram_after[MAX_PACKAGES];
  double psys_before[MAX_PACKAGES], psys_after[MAX_PACKAGES];
  double thermal_spec_power, minimum_power, maximum_power, time_window;
  int j;

  int dram_avail = 0, pp0_avail = 0, pp1_avail = 0, psys_avail = 0;
  int different_units = 0;

  if (cpu_model < 0) {
    printf("\tUnsupported CPU model %d\n", cpu_model);
    return -1;
  }

  switch (cpu_model) {

  case CPU_SANDYBRIDGE_EP:
  case CPU_IVYBRIDGE_EP:
    pp0_avail = 1;
    pp1_avail = 0;
    dram_avail = 1;
    different_units = 0;
    psys_avail = 0;
    break;

  case CPU_HASWELL_EP:
  case CPU_BROADWELL_EP:
  case CPU_SKYLAKE_X:
    pp0_avail = 1;
    pp1_avail = 0;
    dram_avail = 1;
    different_units = 1;
    psys_avail = 0;
    break;

  case CPU_KNIGHTS_LANDING:
  case CPU_KNIGHTS_MILL:
    pp0_avail = 0;
    pp1_avail = 0;
    dram_avail = 1;
    different_units = 1;
    psys_avail = 0;
    break;

  case CPU_SANDYBRIDGE:
  case CPU_IVYBRIDGE:
    pp0_avail = 1;
    pp1_avail = 1;
    dram_avail = 0;
    different_units = 0;
    psys_avail = 0;
    break;

  case CPU_HASWELL:
  case CPU_HASWELL_ULT:
  case CPU_HASWELL_GT3E:
  case CPU_BROADWELL:
  case CPU_BROADWELL_GT3E:
  case CPU_ATOM_GOLDMONT:
  case CPU_ATOM_GEMINI_LAKE:
  case CPU_ATOM_DENVERTON:
    pp0_avail = 1;
    pp1_avail = 1;
    dram_avail = 1;
    different_units = 0;
    psys_avail = 0;
    break;

  case CPU_SKYLAKE:
  case CPU_SKYLAKE_HS:
  case CPU_KABYLAKE:
  case CPU_KABYLAKE_MOBILE:
  case CPU_ROCKETLAKE:
    pp0_avail = 1;
    pp1_avail = 1;
    dram_avail = 1;
    different_units = 0;
    psys_avail = 1;
    break;
  }

  for (j = 0; j < total_packages; j++) {
    printf("\tListing paramaters for package #%d\n", j);

    fd = open_msr(package_map[j]);

    /* Calculate the units used */
    result = read_msr(fd, MSR_RAPL_POWER_UNIT);

    power_units = pow(0.5, (double)(result & 0xf));
    cpu_energy_units[j] = pow(0.5, (double)((result >> 8) & 0x1f));
    time_units = pow(0.5, (double)((result >> 16) & 0xf));

    /* On Haswell EP and Knights Landing */
    /* The DRAM units differ from the CPU ones */
    if (different_units) {
      dram_energy_units[j] = pow(0.5, (double)16);
      printf("DRAM: Using %lf instead of %lf\n", dram_energy_units[j],
             cpu_energy_units[j]);
    } else {
      dram_energy_units[j] = cpu_energy_units[j];
    }

    printf("\t\tPower units = %.3fW\n", power_units);
    printf("\t\tCPU Energy units = %.8fJ\n", cpu_energy_units[j]);
    printf("\t\tDRAM Energy units = %.8fJ\n", dram_energy_units[j]);
    printf("\t\tTime units = %.8fs\n", time_units);
    printf("\n");

    /* Show package power info */
    result = read_msr(fd, MSR_PKG_POWER_INFO);
    thermal_spec_power = power_units * (double)(result & 0x7fff);
    printf("\t\tPackage thermal spec: %.3fW\n", thermal_spec_power);
    minimum_power = power_units * (double)((result >> 16) & 0x7fff);
    printf("\t\tPackage minimum power: %.3fW\n", minimum_power);
    maximum_power = power_units * (double)((result >> 32) & 0x7fff);
    printf("\t\tPackage maximum power: %.3fW\n", maximum_power);
    time_window = time_units * (double)((result >> 48) & 0x7fff);
    printf("\t\tPackage maximum time window: %.6fs\n", time_window);

    /* Show package power limit */
    result = read_msr(fd, MSR_PKG_RAPL_POWER_LIMIT);
    printf("\t\tPackage power limits are %s\n",
           (result >> 63) ? "locked" : "unlocked");
    double pkg_power_limit_1 = power_units * (double)((result >> 0) & 0x7FFF);
    double pkg_time_window_1 = time_units * (double)((result >> 17) & 0x007F);
    printf("\t\tPackage power limit #1: %.3fW for %.6fs (%s, %s)\n",
           pkg_power_limit_1, pkg_time_window_1,
           (result & (1LL << 15)) ? "enabled" : "disabled",
           (result & (1LL << 16)) ? "clamped" : "not_clamped");
    double pkg_power_limit_2 = power_units * (double)((result >> 32) & 0x7FFF);
    double pkg_time_window_2 = time_units * (double)((result >> 49) & 0x007F);
    printf("\t\tPackage power limit #2: %.3fW for %.6fs (%s, %s)\n",
           pkg_power_limit_2, pkg_time_window_2,
           (result & (1LL << 47)) ? "enabled" : "disabled",
           (result & (1LL << 48)) ? "clamped" : "not_clamped");

    /* only available on *Bridge-EP */
    if ((cpu_model == CPU_SANDYBRIDGE_EP) || (cpu_model == CPU_IVYBRIDGE_EP)) {
      result = read_msr(fd, MSR_PKG_PERF_STATUS);
      double acc_pkg_throttled_time = (double)result * time_units;
      printf("\tAccumulated Package Throttled Time : %.6fs\n",
             acc_pkg_throttled_time);
    }

    /* only available on *Bridge-EP */
    if ((cpu_model == CPU_SANDYBRIDGE_EP) || (cpu_model == CPU_IVYBRIDGE_EP)) {
      result = read_msr(fd, MSR_PP0_PERF_STATUS);
      double acc_pp0_throttled_time = (double)result * time_units;
      printf("\tPowerPlane0 (core) Accumulated Throttled Time "
             ": %.6fs\n",
             acc_pp0_throttled_time);

      result = read_msr(fd, MSR_PP0_POLICY);
      int pp0_policy = (int)result & 0x001f;
      printf("\tPowerPlane0 (core) for core %d policy: %d\n", core, pp0_policy);
    }

    if (pp1_avail) {
      result = read_msr(fd, MSR_PP1_POLICY);
      int pp1_policy = (int)result & 0x001f;
      printf("\tPowerPlane1 (on-core GPU if avail) %d policy: %d\n", core,
             pp1_policy);
    }
    close(fd);
  }
  printf("\n");

  for (j = 0; j < total_packages; j++) {

    fd = open_msr(package_map[j]);

    /* Package Energy */
    result = read_msr(fd, MSR_PKG_ENERGY_STATUS);
    package_before[j] = (double)result * cpu_energy_units[j];

    /* PP0 energy */
    /* Not available on Knights* */
    /* Always returns zero on Haswell-EP? */
    if (pp0_avail) {
      result = read_msr(fd, MSR_PP0_ENERGY_STATUS);
      pp0_before[j] = (double)result * cpu_energy_units[j];
    }

    /* PP1 energy */
    /* not available on *Bridge-EP */
    if (pp1_avail) {
      result = read_msr(fd, MSR_PP1_ENERGY_STATUS);
      pp1_before[j] = (double)result * cpu_energy_units[j];
    }

    /* Updated documentation (but not the Vol3B) says Haswell and	*/
    /* Broadwell have DRAM support too				*/
    if (dram_avail) {
      result = read_msr(fd, MSR_DRAM_ENERGY_STATUS);
      dram_before[j] = (double)result * dram_energy_units[j];
    }

    /* Skylake and newer for Psys				*/
    if (psys_avail) {
      result = read_msr(fd, MSR_PLATFORM_ENERGY_STATUS);
      psys_before[j] = (double)result * cpu_energy_units[j];
    }

    close(fd);
  }

  printf("\n\tSleeping 1 second\n\n");
  sleep(1);

  for (j = 0; j < total_packages; j++) {

    fd = open_msr(package_map[j]);

    printf("\tPackage %d:\n", j);

    result = read_msr(fd, MSR_PKG_ENERGY_STATUS);
    package_after[j] = (double)result * cpu_energy_units[j];
    printf("\t\tPackage energy: %.6fJ\n", package_after[j] - package_before[j]);

    result = read_msr(fd, MSR_PP0_ENERGY_STATUS);
    pp0_after[j] = (double)result * cpu_energy_units[j];
    printf("\t\tPowerPlane0 (cores): %.6fJ\n", pp0_after[j] - pp0_before[j]);

    /* not available on SandyBridge-EP */
    if (pp1_avail) {
      result = read_msr(fd, MSR_PP1_ENERGY_STATUS);
      pp1_after[j] = (double)result * cpu_energy_units[j];
      printf("\t\tPowerPlane1 (on-core GPU if avail): %.6f J\n",
             pp1_after[j] - pp1_before[j]);
    }

    if (dram_avail) {
      result = read_msr(fd, MSR_DRAM_ENERGY_STATUS);
      dram_after[j] = (double)result * dram_energy_units[j];
      printf("\t\tDRAM: %.6fJ\n", dram_after[j] - dram_before[j]);
    }

    if (psys_avail) {
      result = read_msr(fd, MSR_PLATFORM_ENERGY_STATUS);
      psys_after[j] = (double)result * cpu_energy_units[j];
      printf("\t\tPSYS: %.6fJ\n", psys_after[j] - psys_before[j]);
    }

    close(fd);
  }
  printf("\n");
  printf("Note: the energy measurements can overflow in 60s or so\n");
  printf("      so try to sample the counters more often than that.\n\n");

  return 0;
}

int main() {
  int core = 0;
  int result = -1;
  int cpu_model;

  opterr = 0;

  cpu_model = cpu::model();
  if (cpu_model == -1) {
    printf("Unsupported CPU model.\n");
    return -1;
  }

  detect_packages();

  result = rapl_msr(core, cpu_model);

  if (result < 0) {
    printf("Unable to read RAPL counters.\n");
    printf("* Verify you have an Intel Sandybridge or newer processor\n");
    printf("* You may need to run as root or have "
           "/proc/sys/kernel/perf_event_paranoid set properly\n");
    printf("* If using raw msr access, make sure msr module is installed\n");
    printf("\n");

    return -1;
  }

  return 0;
}
