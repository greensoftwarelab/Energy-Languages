compile:
	$(SWIFTC) pidigits.swift-2.swift -Ounchecked -whole-module-optimization -I Include/swift/gmp -o pidigits.swift-2.swift_run

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "./pidigits.swift-2.swift_run 10000" Swift pidigits

run:
	./pidigits.swift-2.swift_run 10000

mem:
	/usr/bin/time -v ./pidigits.swift-2.swift_run 10000

valgrind:
	valgrind --tool=massif --stacks=yes ./pidigits.swift-2.swift_run 10000
