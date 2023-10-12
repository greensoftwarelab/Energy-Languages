ifndef GO
GO := go
endif

compile:
	$(GO) build -o mandelbrot_run mandelbrot.go

run:
	env GOGC=off ./mandelbrot_run 16000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../../scripts/RAPL/build/rapl "$(JSON)" env GOGC=off ./mandelbrot_run 16000
