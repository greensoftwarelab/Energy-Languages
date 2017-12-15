compile:
	/usr/local/src/rust-1.16.0/bin/rustc -C opt-level=3 -C target-cpu=core2 -C lto -L /usr/local/src/rust-libs fasta.rs -o fasta.rust-2.rust_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fasta.rust-2.rust_run 25000000" Rust fasta

run:
	./fasta.rust-2.rust_run 25000000

mem:
	/usr/bin/time -v ./fasta.rust-2.rust_run 25000000

valgrind:
	valgrind --tool=massif --stacks=yes ./fasta.rust-2.rust_run 25000000
