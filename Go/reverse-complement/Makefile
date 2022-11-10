ifndef GO
GO := go
endif

compile:
	$(GO) build -o revcomp_run revcomp.go

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp_run 0 < ../../revcomp-input25000000.txt" Go reverse-complement

run:
	./revcomp_run 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v ./revcomp_run 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./revcomp_run 0 < ../../revcomp-input25000000.txt
