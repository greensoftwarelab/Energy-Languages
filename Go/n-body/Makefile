compile:
	/usr/bin/go build -o nbody.go_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./nbody.go_run 50000000" Go n-body

run:
	./nbody.go_run 50000000

mem:
	/usr/bin/time -v ./nbody.go_run 50000000

valgrind:
	valgrind --tool=massif --stacks=yes ./nbody.go_run 50000000
