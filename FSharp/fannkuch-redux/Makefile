compile:
	cp fannkuchredux.fsharpcore-3.fsharpcore Program.fs
	$(DOTNET) build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 12" FSharp fannkuch-redux

run:
	$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 12

mem:
	/usr/bin/time -v  $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 12

valgrind:
	valgrind --tool=massif --stacks=yes  $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 12
