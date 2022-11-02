compile:
	$(SWIFTC) revcomp.swift -Ounchecked -whole-module-optimization  -o revcomp.swift_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp.swift_run 0 < ../../revcomp-input25000000.txt" Swift reverse-complement

run:
	./revcomp.swift_run 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v  ./revcomp.swift_run 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  ./revcomp.swift_run 0 < ../../revcomp-input25000000.txt
