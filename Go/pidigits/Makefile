compile:
	/usr/bin/go build -o pidigits.go-3.go_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./pidigits.go-3.go_run 10000" Go pidigits

run:
	./pidigits.go-3.go_run 10000

mem:
	/usr/bin/time -v ./pidigits.go-3.go_run 10000

valgrind:
	valgrind --tool=massif --stacks=yes ./pidigits.go-3.go_run 10000
