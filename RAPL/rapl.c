#include "rapl.h"


int cpu_model;
int core=0;

double package_before,package_after;
double pp0_before,pp0_after;
double pp1_before=0.0,pp1_after;
double dram_before=0.0,dram_after;

double power_units,energy_units,time_units;

int open_msr(int core) {

  char msr_filename[BUFSIZ];
  int fd;

  sprintf(msr_filename, "/dev/cpu/%d/msr", core);
  fd = open(msr_filename, O_RDONLY);
  if ( fd < 0 ) {
    if ( errno == ENXIO ) {
      fprintf(stderr, "rdmsr: No CPU %d\n", core);
      exit(2);
    } else if ( errno == EIO ) {
      fprintf(stderr, "rdmsr: CPU %d doesn't support MSRs\n", core);
      exit(3);
    } else {
      perror("rdmsr:open");
      fprintf(stderr,"Trying to open %s\n",msr_filename);
      exit(127);
    }
  }

  return fd;
}

long long read_msr(int fd, int which) {

  uint64_t data;

  if ( pread(fd, &data, sizeof data, which) != sizeof data ) {
    perror("rdmsr:pread");
    exit(127);
  }

  return (long long)data;
}

#define CPU_SANDYBRIDGE   42
#define CPU_SANDYBRIDGE_EP  45
#define CPU_IVYBRIDGE   58
#define CPU_IVYBRIDGE_EP  62
#define CPU_HASWELL   60
#define CPU_HASWELL2   69
#define CPU_HASWELL3   70
#define CPU_HASWELL_EP   63
#define CPU_SKYLAKE1   78
#define CPU_SKYLAKE2   94
#define CPU_BROADWELL  77
#define CPU_BROADWELL2  79
#define CPU_KABYLAKE 158

int detect_cpu(void) {

	FILE *fff;

	int family,model=-1;
	char buffer[BUFSIZ],*result;
	char vendor[BUFSIZ];

	fff=fopen("/proc/cpuinfo","r");
	if (fff==NULL) return -1;

	while(1) {
		result=fgets(buffer,BUFSIZ,fff);
		if (result==NULL) break;

		if (!strncmp(result,"vendor_id",8)) {
			sscanf(result,"%*s%*s%s",vendor);

			if (strncmp(vendor,"GenuineIntel",12)) {
				printf("%s not an Intel chip\n",vendor);
				return -1;
			}
		}

		if (!strncmp(result,"cpu family",10)) {
			sscanf(result,"%*s%*s%*s%d",&family);
			if (family!=6) {
				printf("Wrong CPU family %d\n",family);
				return -1;
			}
		}

		if (!strncmp(result,"model",5)) {
			sscanf(result,"%*s%*s%d",&model);
		}

	}

	fclose(fff);
/**
	switch(model) {
		case CPU_SANDYBRIDGE:
			printf("Found Sandybridge CPU\n");
			break;
		case CPU_SANDYBRIDGE_EP:
			printf("Found Sandybridge-EP CPU\n");
			break;
		case CPU_IVYBRIDGE:
			printf("Found Ivybridge CPU\n");
			break;
		case CPU_IVYBRIDGE_EP:
			printf("Found Ivybridge-EP CPU\n");
			break;
    case CPU_HASWELL:
      printf("Found Haswell CPU\n");
      break;
    case CPU_HASWELL2:
      printf("Found Haswell2 CPU\n");
      break;
    case CPU_HASWELL3:
      printf("Found Haswell3 CPU\n");
      break;
    case CPU_HASWELL_EP:
      printf("Found Haswell_EP CPU\n");
      break;
    case CPU_SKYLAKE1:
      printf("Found SKYLAKE1 CPU\n");
      break;
    case CPU_SKYLAKE2:
      printf("Found SKYLAKE2 CPU\n");
      break;
    case CPU_BROADWELL:
      printf("Found BROADWELL CPU\n");
      break;
    case CPU_BROADWELL2:
      printf("Found BROADWELL2 CPU\n");
      break;
    case CPU_KABYLAKE:
      printf("Found KABYLAKE CPU\n");
      break;
		default:	printf("Unsupported model %d\n",model);
				//model=-1;
				break;
	}
**/
	return model;
}






int rapl_init(int core)
{ int fd;
  long long result;

  cpu_model=detect_cpu();
  if (cpu_model<0) {
  printf("Unsupported CPU type\n");
  return -1;
  }

  // printf("Checking core #%d\n",core);

  fd=open_msr(core);

  /* Calculate the units used */
  result=read_msr(fd,MSR_RAPL_POWER_UNIT);

  power_units=pow(0.5,(double)(result&0xf));
  energy_units=pow(0.5,(double)((result>>8)&0x1f));
  time_units=pow(0.5,(double)((result>>16)&0xf));

  /*
  printf("Power units = %.3fW\n",power_units);
  printf("Energy units = %.8fJ\n",energy_units);
  printf("Time units = %.8fs\n",time_units);
  printf("\n");
  */


  return 0;
}


void show_power_info(int core)
{ int fd;
  long long result;
  double thermal_spec_power,minimum_power,maximum_power,time_window;



 /* Show package power info */

  fd=open_msr(core);
  result=read_msr(fd,MSR_PKG_POWER_INFO);

  thermal_spec_power=power_units*(double)(result&0x7fff);
  printf("Package thermal spec: %.3fW\n",thermal_spec_power);

  minimum_power=power_units*(double)((result>>16)&0x7fff);
  printf("Package minimum power: %.3fW\n",minimum_power);

  maximum_power=power_units*(double)((result>>32)&0x7fff);
  printf("Package maximum power: %.3fW\n",maximum_power);

  time_window=time_units*(double)((result>>48)&0x7fff);
  printf("Package maximum time window: %.6fs\n",time_window);
}



void show_power_limit(int core)
{ int fd;
  long long result;


 /* Show package power limit */

  fd=open_msr(core);
  result=read_msr(fd,MSR_PKG_RAPL_POWER_LIMIT);

  printf("Package power limits are %s\n", (result >> 63) ? "locked" : "unlocked");
  double pkg_power_limit_1 = power_units*(double)((result>>0)&0x7FFF);
  double pkg_time_window_1 = time_units*(double)((result>>17)&0x007F);
  printf("Package power limit #1: %.3fW for %.6fs (%s, %s)\n", pkg_power_limit_1, pkg_time_window_1,
           (result & (1LL<<15)) ? "enabled" : "disabled",
           (result & (1LL<<16)) ? "clamped" : "not_clamped");
  double pkg_power_limit_2 = power_units*(double)((result>>32)&0x7FFF);
  double pkg_time_window_2 = time_units*(double)((result>>49)&0x007F);
  printf("Package power limit #2: %.3fW for %.6fs (%s, %s)\n", pkg_power_limit_2, pkg_time_window_2,
          (result & (1LL<<47)) ? "enabled" : "disabled",
          (result & (1LL<<48)) ? "clamped" : "not_clamped");

  printf("\n");

}




void rapl_before(FILE * fp,int core)
{ int fd;
  long long result;


  fd=open_msr(core);
  result=read_msr(fd,MSR_PKG_ENERGY_STATUS);

  package_before=(double)result*energy_units;
  //  fprintf(fp,"Package energy: %.6fJ\n",package_before);

  /* only available on *Bridge-EP */
  if ((cpu_model==CPU_SANDYBRIDGE_EP) || (cpu_model==CPU_IVYBRIDGE_EP))
  {
    result=read_msr(fd,MSR_PKG_PERF_STATUS);
    double acc_pkg_throttled_time=(double)result*time_units;
    // fprintf(fp,"Accumulated Package Throttled Time : %.6fs\n",acc_pkg_throttled_time);
  }

  result=read_msr(fd,MSR_PP0_ENERGY_STATUS);
  pp0_before=(double)result*energy_units;
  // fprintf(fp,"PowerPlane0 (core) for core %d energy before: %.6fJ\n",core,pp0_before);

  result=read_msr(fd,MSR_PP0_POLICY);
  int pp0_policy=(int)result&0x001f;
  // fprintf(fp,"PowerPlane0 (core) for core %d policy: %d\n",core,pp0_policy);

  /* only available on *Bridge-EP */
  if ((cpu_model==CPU_SANDYBRIDGE_EP) || (cpu_model==CPU_IVYBRIDGE_EP))
  {
    result=read_msr(fd,MSR_PP0_PERF_STATUS);
    double acc_pp0_throttled_time=(double)result*time_units;
    // fprintf(fp,"PowerPlane0 (core) Accumulated Throttled Time : %.6fs\n",acc_pp0_throttled_time);
  }

  /* not available on *Bridge-EP */
  if ((cpu_model==CPU_SANDYBRIDGE) || (cpu_model==CPU_IVYBRIDGE) ||
  (cpu_model==CPU_HASWELL)) {
     result=read_msr(fd,MSR_PP1_ENERGY_STATUS);
     pp1_before=(double)result*energy_units;
     // fprintf(fp,"PowerPlane1 (on-core GPU if avail) before: %.6fJ\n",pp1_before);
     result=read_msr(fd,MSR_PP1_POLICY);
     int pp1_policy=(int)result&0x001f;
     //fprintf(fp,"PowerPlane1 (on-core GPU if avail) %d policy: %d\n",core,pp1_policy);
  }

  /* Despite documentation saying otherwise, it looks like */
  /* You can get DRAM readings on regular Haswell          */
  if ((cpu_model==CPU_SANDYBRIDGE_EP) || (cpu_model==CPU_IVYBRIDGE_EP) ||
  (cpu_model==CPU_HASWELL)) {
     result=read_msr(fd,MSR_DRAM_ENERGY_STATUS);
     dram_before=(double)result*energy_units;
     // fprintf(fp,"DRAM energy before: %.6fJ\n",dram_before);
  }

}


void rapl_after(FILE * fp , int core)
{ int fd;
  long long result;

  fd=open_msr(core);

  result=read_msr(fd,MSR_PKG_ENERGY_STATUS);
  package_after=(double)result*energy_units;
  //  fprintf(fp,"Package energy: %.6fJ consumed\n",package_after-package_before);
  fprintf(fp,"%.18f, ",package_after-package_before);  // PACKAGE

  result=read_msr(fd,MSR_PP0_ENERGY_STATUS);
  pp0_after=(double)result*energy_units;

  fprintf(fp,"%.18f, ",pp0_after-pp0_before);    // CORE


  /* not available on SandyBridge-EP */
  if ((cpu_model==CPU_SANDYBRIDGE) || (cpu_model==CPU_IVYBRIDGE) ||
  (cpu_model==CPU_HASWELL)) {
     result=read_msr(fd,MSR_PP1_ENERGY_STATUS);
     pp1_after=(double)result*energy_units;
     fprintf(fp,"%.18f, ",pp1_after-pp1_before);     // GPU
  }
  else
    fprintf(fp," , ");

  if ((cpu_model==CPU_SANDYBRIDGE_EP) || (cpu_model==CPU_IVYBRIDGE_EP) ||
  (cpu_model==CPU_HASWELL)) {
     result=read_msr(fd,MSR_DRAM_ENERGY_STATUS);
     dram_after=(double)result*energy_units;
     fprintf(fp,"%.18f, ",dram_after-dram_before);     // DRAM
  }
  else
    fprintf(fp," , ");  

}
