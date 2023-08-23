ifndef NODE
NODE := node
endif

compile:

run:
	$(NODE) fannkuchredux.js 12

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" $(NODE) fannkuchredux.js 12
