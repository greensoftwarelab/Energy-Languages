compile:
	/usr/local/src/swift-2017-04-13-a-ubuntu16.10/usr/bin/swiftc nbody.swift-5.swift -Ounchecked -whole-module-optimization  -o nbody.swift-5.swift_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./nbody.swift-5.swift_run 50000000" Swift n-body

run:
	./nbody.swift-5.swift_run 50000000

mem:
	/usr/bin/time -v  ./nbody.swift-5.swift_run 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  ./nbody.swift-5.swift_run 50000000
