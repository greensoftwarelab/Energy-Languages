ifndef GO
GO := go
endif

compile:
	$(GO) build -o regexredux_run regexredux.go

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./regexredux_run 0 < ../../regexredux-input5000000.txt" Go regex-redux

run:
	./regexredux_run 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v ./regexredux_run 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./regexredux_run 0 < ../../regexredux-input5000000.txt
