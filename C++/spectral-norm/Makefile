-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -mfpmath=sse -msse2 -fopenmp spectralnorm.cpp -o spectralnorm_run

run:
	./spectralnorm_run 5500

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./spectralnorm_run 5500
