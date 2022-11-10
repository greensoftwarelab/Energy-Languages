ifndef GO
GO := go
endif

compile:
	$(GO) build -o binarytrees_run binarytrees.go

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees_run 21" Go binary-trees

run:
	./binarytrees_run 21

mem:
	/usr/bin/time -v ./binarytrees_run 21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees_run 21
