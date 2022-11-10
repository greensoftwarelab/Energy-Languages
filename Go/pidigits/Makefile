ifndef GO
GO := go
endif

compile:
	$(GO) build -o pidigits_run pidigits.go

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./pidigits_run 10000" Go pidigits

run:
	./pidigits_run 10000

mem:
	/usr/bin/time -v ./pidigits_run 10000

valgrind:
	valgrind --tool=massif --stacks=yes ./pidigits_run 10000
