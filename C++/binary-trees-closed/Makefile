-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) binarytrees.cpp -o binarytrees_run

run:
	./binarytrees_run 21

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) './binarytrees_run 21'
