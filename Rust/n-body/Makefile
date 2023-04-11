-include ../Makefile.defaults

compile:
	RUSTFLAGS="$(RUSTFLAGS) -C llvm-args=-unroll-threshold=500" cargo build --release

run:
	./target/release/n-body 50000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) './target/release/n-body 50000000'
