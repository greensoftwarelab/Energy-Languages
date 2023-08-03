-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -mfpmath=sse -msse3 nbody.c -o nbody_run -lm

run:
	./nbody_run 50000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl $(JSON) ./nbody_run 50000000
