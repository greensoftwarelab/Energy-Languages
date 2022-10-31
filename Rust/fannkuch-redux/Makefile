compile:
	$(RUSTC) -C opt-level=3 -C target-cpu=core2 -C lto -L $(RUSTLIB) fannkuchredux.rs -o fannkuchredux.rust-3.rust_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux.rust-3.rust_run 12" Rust fannkuch-redux

run:
	./fannkuchredux.rust-3.rust_run 12

mem:
	/usr/bin/time -v ./fannkuchredux.rust-3.rust_run 12

valgrind:
	valgrind --tool=massif --stacks=yes ./fannkuchredux.rust-3.rust_run 12
