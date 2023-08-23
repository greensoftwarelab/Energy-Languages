-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) pidigits.cpp -o pidigits_run -lgmp -lgmpxx

run:
	./pidigits_run 10000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./pidigits_run 10000
