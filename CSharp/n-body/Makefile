compile:
	cp nbody.csharpcore-3.csharpcore Program.cs
	/usr/bin/dotnet build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 50000000" CSharp n-body

run:
	/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 50000000

mem:
	/usr/bin/time -v  /usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 50000000
