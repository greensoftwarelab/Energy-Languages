compile:
	/usr/local/src/rust-1.16.0/bin/rustc -C opt-level=3 -C target-cpu=core2 -C lto -L /usr/local/src/rust-libs mandelbrot.rs -o mandelbrot.rust-4.rust_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot.rust-4.rust_run 16000" Rust mandelbrot

run:
	./mandelbrot.rust-4.rust_run 16000

mem:
	/usr/bin/time -v  ./mandelbrot.rust-4.rust_run 16000

valgrind:
	valgrind --tool=massif --stacks=yes  ./mandelbrot.rust-4.rust_run 16000
