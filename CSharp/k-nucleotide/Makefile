compile:
	cp knucleotide.csharpcore-6.csharpcore Program.cs
	$(DOTNET) build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../knucleotide-input25000000.txt" CSharp k-nucleotide

run:
	$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../knucleotide-input25000000.txt

mem:
	/usr/bin/time -v $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../knucleotide-input25000000.txt
