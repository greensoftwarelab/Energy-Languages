compile:
	/usr/bin/go build -o mandelbrot.go-3.go_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot.go-3.go_run 16000" Go mandelbrot

run:
	./mandelbrot.go-3.go_run 16000

mem:
	/usr/bin/time -v ./mandelbrot.go-3.go_run 16000

valgrind:
	valgrind --tool=massif --stacks=yes ./mandelbrot.go-3.go_run 16000
