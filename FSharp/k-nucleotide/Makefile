compile:
	cp knucleotide.fsharpcore-4.fsharpcore Program.fs
	$(DOTNET) build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../knucleotide-input25000000.txt" FSharp k-nucleotide

run:
	$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../knucleotide-input25000000.txt

mem:
	/usr/bin/time -v  $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../knucleotide-input25000000.txt
