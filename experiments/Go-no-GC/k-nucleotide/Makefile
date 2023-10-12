ifndef GO
GO := go
endif

compile:
	$(GO) build -o knucleotide_run knucleotide.go

run:
	./knucleotide_run 0 < ../../knucleotide-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./knucleotide_run 0 < ../../knucleotide-input25000000.txt
