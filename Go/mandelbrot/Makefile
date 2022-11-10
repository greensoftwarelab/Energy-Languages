ifndef GO
GO := go
endif

compile:
	$(GO) build -o mandelbrot_run mandelbrot.go

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot_run 16000" Go mandelbrot

run:
	./mandelbrot_run 16000

mem:
	/usr/bin/time -v ./mandelbrot_run 16000

valgrind:
	valgrind --tool=massif --stacks=yes ./mandelbrot_run 16000
