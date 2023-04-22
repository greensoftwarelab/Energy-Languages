-include ../Makefile.defaults

compile:
	cmake . -B build -DCMAKE_BUILD_TYPE=Release
	cmake --build build

run:
	./build/regexredux_run 0 < ../../regexredux-input5000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) ./regexredux_run 0 < ../../regexredux-input5000000.txt
