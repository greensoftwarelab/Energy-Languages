ifndef GO
GO := go
endif

compile:
	$(GO) build -o fasta_run fasta.go

run:
	env GOGC=off ./fasta_run 25000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../../scripts/RAPL/build/rapl "$(JSON)" env GOGC=off ./fasta_run 25000000
