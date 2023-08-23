ifndef NODE
NODE := node
endif

compile:

run:
	$(NODE) revcomp.js 0 < ../../revcomp-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" $(NODE) revcomp.js 0 < ../../revcomp-input25000000.txt
