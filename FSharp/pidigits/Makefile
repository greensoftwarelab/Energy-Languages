compile:	
	cp pidigits.fsharpcore-3.fsharpcore Program.fs
	/usr/bin/dotnet build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 10000" FSharp pidigits

run:
	/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 10000

mem:
	/usr/bin/time -v /usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 10000

valgrind:
	valgrind --tool=massif --stacks=yes ./usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 10000
