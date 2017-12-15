compile:
	/usr/local/src/swift-2017-04-13-a-ubuntu16.10/usr/bin/swiftc binarytrees.swift-9.swift -Ounchecked -whole-module-optimization -I Include/swift/apr -o binarytrees.swift-9.swift_run

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees.swift-9.swift_run 21" Swift binary-trees

run:
	./binarytrees.swift-9.swift_run 21

mem:
	/usr/bin/time -v ./binarytrees.swift-9.swift_run 21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees.swift-9.swift_run 21
