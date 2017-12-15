compile:
	cp threadring.csharpcore Program.cs
	/usr/bin/dotnet build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 50000000"

run:
	/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 50000000