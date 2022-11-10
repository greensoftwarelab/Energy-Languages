ifndef GO
GO := go
endif

compile:
	$(GO) build -o spectralnorm_run spectralnorm.go

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm_run 5500" Go spectral-norm

run:
	./spectralnorm_run 5500

mem:
	/usr/bin/time -v ./spectralnorm_run 5500

valgrind:
	valgrind --tool=massif --stacks=yes ./spectralnorm_run 5500
