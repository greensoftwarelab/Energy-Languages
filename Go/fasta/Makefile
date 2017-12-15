compile:
	/usr/bin/go build -o fasta.go-3.go_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fasta.go-3.go_run 25000000" Go fasta

run:
	./fasta.go-3.go_run 25000000

mem:
	/usr/bin/time -v ./fasta.go-3.go_run 25000000

valgrind:
	valgrind --tool=massif --stacks=yes ./fasta.go-3.go_run 25000000
