compile:
	cp fannkuchredux.fsharpcore-3.fsharpcore Program.fs
	/usr/bin/dotnet build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 12" FSharp fannkuch-redux

run:
	/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 12

mem:
	/usr/bin/time -v  /usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 12

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 12
