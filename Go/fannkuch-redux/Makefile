compile:
	/usr/bin/go build -o fannkuchredux.go_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux.go_run 12" Go fannkuch-redux

run:
	./fannkuchredux.go_run 12

mem:
	/usr/bin/time -v ./fannkuchredux.go_run 12

valgrind:
	valgrind --tool=massif --stacks=yes ./fannkuchredux.go_run 12
