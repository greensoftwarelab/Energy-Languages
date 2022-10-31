compile:
	$(RUSTC) -C opt-level=3 -C target-cpu=core2 -C lto -L $(RUSTLIB) spectralnorm.rs -o spectralnorm.rust-4.rust_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm.rust-4.rust_run 5500" Rust spectral-norm

run:
	./spectralnorm.rust-4.rust_run 5500

mem:
	/usr/bin/time -v ./spectralnorm.rust-4.rust_run 5500

valgrind:
	valgrind --tool=massif --stacks=yes ./spectralnorm.rust-4.rust_run 5500
