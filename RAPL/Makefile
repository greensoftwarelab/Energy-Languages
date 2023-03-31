CXXFLAGS := -O3 -march=native -mtune=native -DNDEBUG -Wall -Wextra -Wpedantic -Werror
LDLIBS := -lm

main: rapl-read.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)

clean:
	rm -rf main.o rapl.o
