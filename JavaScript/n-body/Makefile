ifndef NODE
NODE := node
endif

compile:

run:
	$(NODE) nbody.js 50000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) $(NODE) nbody.js 50000000
