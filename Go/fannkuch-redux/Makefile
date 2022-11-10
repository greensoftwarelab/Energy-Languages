ifndef GO
GO := go
endif

compile:
	$(GO) build -o fannkuchredux_run fannkuchredux.go

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux_run 12" Go fannkuch-redux

run:
	./fannkuchredux_run 12

mem:
	/usr/bin/time -v ./fannkuchredux_run 12

valgrind:
	valgrind --tool=massif --stacks=yes ./fannkuchredux_run 12
