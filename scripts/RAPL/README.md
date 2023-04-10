# RAPL

This utility was originally adapted from [`rapl-read.c` available on Vince Weaver's website](https://web.eece.maine.edu/~vweaver/projects/rapl/).

## Build

```
[scripts/RAPL] sudo modprobe msr
[scripts/RAPL] sudo cmake . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
[scripts/RAPL] sudo chown -R $(whoami) build
[scripts/RAPL] cmake --build build
```

`sudo` is required when configuring as we check which RAPL MSRs can be read from at this step.
