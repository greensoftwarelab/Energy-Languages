compile:
	$(CXX) -pipe -O3 -fomit-frame-pointer -march=native -std=c++11 -mtune=native -mfpmath=sse -msse2 revcomp.gpp-4.c++ -o revcomp.gpp-4.gpp_run -lpthread

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp.gpp-4.gpp_run 0 < ../../revcomp-input25000000.txt" C++ reverse-complement

run:
	./revcomp.gpp-4.gpp_run 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v ./revcomp.gpp-4.gpp_run 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./revcomp.gpp-4.gpp_run 0 < ../../revcomp-input25000000.txt
