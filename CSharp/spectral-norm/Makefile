compile:
	cp spectralnorm.csharpcore-3.csharpcore Program.cs
	$(DOTNET) build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 5500" CSharp spectral-norm

run:
	$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 5500

mem:
	/usr/bin/time -v  $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 5500

valgrind:
	valgrind --tool=massif --stacks=yes  $(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 5500
