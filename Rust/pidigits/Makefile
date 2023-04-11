-include ../Makefile.defaults

compile:
	RUSTFLAGS="$(RUSTFLAGS)" cargo build --release

run:
	./target/release/pidigits 10000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) './target/release/pidigits 10000'
