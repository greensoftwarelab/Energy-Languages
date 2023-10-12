ifndef GO
GO := go
endif

compile:
	$(GO) build -o spectralnorm_run spectralnorm.go

run:
	env GOGC=off ./spectralnorm_run 5500

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../../scripts/RAPL/build/rapl "$(JSON)" env GOGC=off ./spectralnorm_run 5500
