ifndef GO
GO := go
endif

compile:
	$(GO) build -o mandelbrot_run mandelbrot.go

run:
	./mandelbrot_run 16000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./mandelbrot_run 16000
