# Energy Efficiency in Programming Languages

TODO.

## Documentation

### Requirements

Running the measurement tool requires:
 -  an Intel processor with RAPL support.
 -  Linux (tested on 22.04).

### Docker

The easiest way to run these benchmarks is using Docker.
```bash
% sudo modprobe msr # Enable msr kernel module.
% sudo docker build -f docker/main.Dockerfile -t energy-languages .
% sudo docker run --privileged -v `pwd`/data/tmp:/root/data energy-languages [OPTIONS]
```

The following options are available:
 -  `--languages`: A whitespace-separated list of languages to benchmarks.
 -  `--warmup`: The number of warmup iterations to run before measuring.
 -  `--iterations`: The number of iterations to run for each benchmark.
 -  `--timeout`: The timeout after which to stop execution. Some benchmarks are known to occasionally run indefinitely.

Here is an example running all languages/benchmarks pairs.
```bash
% sudo docker run --privileged -v `pwd`/data/tmp:/root/data energy-languages \
    --languages C C++ Rust Go Java C\# JavaScript TypeScript PHP Python \
	--warmup 3 \
	--iterations 21 \
	--timeout 600
```
