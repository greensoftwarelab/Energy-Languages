compile:
	/usr/local/src/rust-1.16.0/bin/rustc -C opt-level=3 -C target-cpu=core2 -C lto -L /usr/local/src/rust-libs regexredux.rs -o regexredux.rust_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./regexredux.rust_run 0 < ../../regexredux-input5000000.txt" Rust regex-redux

run:
	./regexredux.rust_run 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v ./regexredux.rust_run 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./regexredux.rust_run 0 < ../../regexredux-input5000000.txt
