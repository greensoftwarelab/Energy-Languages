compile:
	/usr/bin/go build -o revcomp.go-6.go_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp.go-6.go_run 0 < ../../revcomp-input25000000.txt" Go reverse-complement

run:
	./revcomp.go-6.go_run 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v   ./revcomp.go-6.go_run 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes   ./revcomp.go-6.go_run 0 < ../../revcomp-input25000000.txt
