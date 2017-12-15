compile:
	/usr/local/src/rust-1.16.0/bin/rustc -C opt-level=3 -C target-cpu=core2 -C lto -L /usr/local/src/rust-libs knucleotide.rs -o knucleotide.rust-4.rust_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./knucleotide.rust-4.rust_run 0 < knucleotide-input25000000.txt" Rust k-nucleotide

run:
	./knucleotide.rust-4.rust_run 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v ./knucleotide.rust-4.rust_run 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./knucleotide.rust-4.rust_run 0 < knucleotide-input25000000.txt
