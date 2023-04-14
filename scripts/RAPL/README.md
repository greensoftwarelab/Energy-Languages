# RAPL

## Build

```
[scripts/RAPL] sudo modprobe msr
[scripts/RAPL] sudo cmake . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
[scripts/RAPL] sudo cmake --build build
```

`sudo` is required when configuring as we check which RAPL MSRs can be read from at this step.
