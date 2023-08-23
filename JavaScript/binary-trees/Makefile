ifndef NODE
NODE := node
endif

compile:

run:
	$(NODE) binarytrees.js 21

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" $(NODE) binarytrees.js 21
