compile:	
	cp spectralnorm.fsharpcore-2.fsharpcore Program.fs
	$(DOTNET) build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 5500" FSharp spectral-norm

run:
	$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 5500

mem:
	/usr/bin/time -v  $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 5500

valgrind:
	valgrind --tool=massif --stacks=yes  $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 5500
