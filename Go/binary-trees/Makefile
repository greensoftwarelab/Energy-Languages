compile:
	/usr/bin/go build -o binarytrees.go-2.go_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees.go-2.go_run 21" Go binary-trees

run:
	./binarytrees.go-2.go_run 21

mem:
	/usr/bin/time -v ./binarytrees.go-2.go_run 21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees.go-2.go_run 21
