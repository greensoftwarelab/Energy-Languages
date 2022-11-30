compile:
	cp threadring.csharpcore Program.cs
	$(DOTNET) build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 50000000"

run:
	$(DOTNET) ./bin/Release/netcoreapp1.1/tmp.dll 50000000