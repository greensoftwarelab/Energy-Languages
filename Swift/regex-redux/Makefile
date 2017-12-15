compile:
	/usr/local/src/swift-2017-04-13-a-ubuntu16.10/usr/bin/swiftc regexredux.swift-2.swift -Ounchecked -whole-module-optimization  -o regexredux.swift-2.swift_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./regexredux.swift-2.swift_run 0 < ../../regexredux-input5000000.txt" Swift regex-redux

run:
	./regexredux.swift-2.swift_run 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v ./regexredux.swift-2.swift_run 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./regexredux.swift-2.swift_run 0 < ../../regexredux-input5000000.txt
