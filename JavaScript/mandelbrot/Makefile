ifndef NODE
NODE := node
endif

compile:

run:
	$(NODE) mandelbrot.js 16000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" $(NODE) mandelbrot.js 16000
