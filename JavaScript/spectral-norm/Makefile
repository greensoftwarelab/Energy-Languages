ifndef NODE
NODE := node
endif

compile:

run:
	$(NODE) spectralnorm.js 5500

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" $(NODE) spectralnorm.js 5500
