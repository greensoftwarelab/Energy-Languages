compile:
	/usr/local/src/rust-1.16.0/bin/rustc -C opt-level=3 -C target-cpu=core2 -C lto -C llvm-args='-unroll-threshold=500' nbody.rs -o nbody.rust-2.rust_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./nbody.rust-2.rust_run 50000000" Rust n-body

run:
	./nbody.rust-2.rust_run 50000000

mem:
	/usr/bin/time -v ./nbody.rust-2.rust_run 50000000

valgrind:
	valgrind --tool=massif --stacks=yes ./nbody.rust-2.rust_run 50000000
