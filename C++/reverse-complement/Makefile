-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -pthread -mfpmath=sse -msse2 revcomp.cpp -o revcomp_run

run:
	./revcomp_run 0 < ../../revcomp-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./revcomp_run 0 < ../../revcomp-input25000000.txt
