CXXFLAGS := --std=c++20 -O3 -march=native -mtune=native -DNDEBUG -Wall -Wextra -Wpedantic -Werror
LDLIBS := -lm

BUILDDIR := build

.PHONY: all
all: $(BUILDDIR)/rapl

$(BUILDDIR)/rapl: rapl-read.cpp
	mkdir -p $(BUILDDIR)
	$(CXX) $(CXXFLAGS) -Iinclude $(LDFLAGS) -o $@ $< $(LDLIBS)

.PHONY: clean
clean:
	rm -rf $(BUILDDIR)
