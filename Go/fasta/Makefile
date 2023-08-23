ifndef GO
GO := go
endif

compile:
	$(GO) build -o fasta_run fasta.go

run:
	./fasta_run 25000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./fasta_run 25000000
