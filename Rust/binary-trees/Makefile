-include ../Makefile.defaults

compile:
	RUSTFLAGS="$(RUSTFLAGS)" cargo build --release

run:
	./target/release/binary-trees 21

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) './target/release/binary-trees 21'
