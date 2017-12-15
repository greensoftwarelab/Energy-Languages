compile:
	/usr/local/src/swift-2017-04-13-a-ubuntu16.10/usr/bin/swiftc mandelbrot.swift-3.swift -Ounchecked -whole-module-optimization  -o mandelbrot.swift-3.swift_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot.swift-3.swift_run 16000" Swift mandelbrot

run:
	./mandelbrot.swift-3.swift_run 16000

mem:
	/usr/bin/time -v  ./mandelbrot.swift-3.swift_run 16000

valgrind:
	valgrind --tool=massif --stacks=yes  ./mandelbrot.swift-3.swift_run 16000
