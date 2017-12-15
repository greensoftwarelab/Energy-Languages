compile:
	/usr/local/src/swift-2017-04-13-a-ubuntu16.10/usr/bin/swiftc spectralnorm.swift-3.swift -Ounchecked -whole-module-optimization  -o spectralnorm.swift-3.swift_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm.swift-3.swift_run 5500" Swift spectral-norm

run:
	./spectralnorm.swift-3.swift_run 5500

mem:
	/usr/bin/time -v ./spectralnorm.swift-3.swift_run 5500

valgrind:
	valgrind --tool=massif --stacks=yes ./spectralnorm.swift-3.swift_run 5500

