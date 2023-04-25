ifndef GO
GO := go
endif

compile:
	$(GO) build -o revcomp_run revcomp.go

run:
	./revcomp_run 0 < ../../revcomp-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) ./revcomp_run 0 < ../../revcomp-input25000000.txt
