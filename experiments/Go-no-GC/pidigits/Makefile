ifndef GO
GO := go
endif

compile:
	$(GO) build -o pidigits_run pidigits.go

run:
	./pidigits_run 10000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./pidigits_run 10000
