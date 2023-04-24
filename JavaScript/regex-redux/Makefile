ifndef NODE
NODE := node
endif

compile:

run:
	$(NODE) regexredux.js 0 < ../../regexredux-input5000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) $(NODE) regexredux.js 0 < ../../regexredux-input5000000.txt
