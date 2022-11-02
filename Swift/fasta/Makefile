compile:
	$(SWIFTC) fasta.swift-3.swift -Ounchecked -whole-module-optimization  -o fasta.swift-3.swift_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fasta.swift-3.swift_run 25000000" Swift fasta

run:
	./fasta.swift-3.swift_run 25000000

mem:
	/usr/bin/time -v  ./fasta.swift-3.swift_run 25000000

valgrind:
	valgrind --tool=massif --stacks=yes  ./fasta.swift-3.swift_run 25000000
