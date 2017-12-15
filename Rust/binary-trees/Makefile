compile:
	/usr/local/src/rust-1.16.0/bin/rustc -C opt-level=3 -C target-cpu=core2 -C lto -L /usr/local/src/rust-libs binarytrees.rs -o binarytrees.rust-2.rust_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees.rust-2.rust_run 21" Rust binary-trees

run:
	./binarytrees.rust-2.rust_run 21

mem:
	/usr/bin/time -v ./binarytrees.rust-2.rust_run 21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees.rust-2.rust_run 21 
