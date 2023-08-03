-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -pthread chameneosredux.c -o chameneosredux_run

run:
	./chameneosredux_run 6000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl $(JSON) ./chameneosredux_run 6000000
