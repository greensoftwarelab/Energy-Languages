compile:
	cp binarytrees.fsharpcore-3.fsharpcore Program.fs
	$(DOTNET) build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 21" FSharp binary-trees

run:
	$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 21

mem:
	/usr/bin/time -v $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 21

valgrind:
	valgrind --tool=massif --stacks=yes $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 21
