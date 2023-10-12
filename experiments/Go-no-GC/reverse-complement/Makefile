ifndef GO
GO := go
endif

compile:
	$(GO) build -o revcomp_run revcomp.go

run:
	env GOGC=off ./revcomp_run 0 < ../../../revcomp-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../../scripts/RAPL/build/rapl "$(JSON)" env GOGC=off ./revcomp_run 0 < ../../../revcomp-input25000000.txt
