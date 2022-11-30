compile:
	cp fasta.fsharpcore Program.fs
	$(DOTNET) build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 25000000" FSharp fasta

run:
	$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 25000000

mem:
	/usr/bin/time -v $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 25000000

valgrind:
	valgrind --tool=massif --stacks=yes $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 25000000
