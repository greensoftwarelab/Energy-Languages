ifndef GO
GO := go
endif

compile:
	$(GO) build -o nbody_run nbody.go

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./nbody_run 50000000" Go n-body

run:
	./nbody_run 50000000

mem:
	/usr/bin/time -v ./nbody_run 50000000

valgrind:
	valgrind --tool=massif --stacks=yes ./nbody_run 50000000
