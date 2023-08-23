ifndef GO
GO := go
endif

compile:
	$(GO) build -o nbody_run nbody.go

run:
	./nbody_run 50000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./nbody_run 50000000
