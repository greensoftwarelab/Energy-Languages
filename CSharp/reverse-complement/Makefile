compile:
	cp revcomp.csharpcore-2.csharpcore Program.cs
	/usr/bin/dotnet build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../revcomp-input25000000.txt" CSharp reverse-complement

run:
	/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v  /usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes   /usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../revcomp-input25000000.txt
