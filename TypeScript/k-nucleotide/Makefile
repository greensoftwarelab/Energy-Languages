ifndef TSC
TSC := tsc
endif

ifndef NODE
NODE := node
endif

compile:
	npm install
	$(TSC) --alwaysStrict -t ESNEXT knucleotide.ts

run:
	$(NODE) knucleotide.js 0 < ../../knucleotide-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" $(NODE) knucleotide.js 0 < ../../knucleotide-input25000000.txt
