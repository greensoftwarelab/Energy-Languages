compile:
	/usr/local/src/rust-1.16.0/bin/rustc -C opt-level=3 -C target-cpu=core2 -C lto  pidigits.rs -o pidigits.rust-2.rust_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./pidigits.rust-2.rust_run 10000" Rust pidigits

run:
	./pidigits.rust-2.rust_run 10000

mem:
	/usr/bin/time -v ./pidigits.rust-2.rust_run 10000

valgrind:
	valgrind --tool=massif --stacks=yes ./pidigits.rust-2.rust_run 10000
