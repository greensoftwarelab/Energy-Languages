ifndef GO
GO := go
endif

compile:
	$(GO) build -o fasta_run fasta.go

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fasta_run 25000000" Go fasta

run:
	./fasta_run 25000000

mem:
	/usr/bin/time -v ./fasta_run 25000000

valgrind:
	valgrind --tool=massif --stacks=yes ./fasta_run 25000000
