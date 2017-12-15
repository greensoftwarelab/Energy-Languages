compile:
	/usr/bin/go build -o spectralnorm.go-4.go_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm.go-4.go_run 5500" Go spectral-norm

run:
	./spectralnorm.go-4.go_run 5500

mem:
	/usr/bin/time -v  ./spectralnorm.go-4.go_run 5500

valgrind:
	valgrind --tool=massif --stacks=yes  ./spectralnorm.go-4.go_run 5500
