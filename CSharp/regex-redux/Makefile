compile:
	cp regexredux.csharpcore-4.csharpcore Program.cs
	/usr/bin/dotnet build -c Release

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../regexredux-input5000000.txt" CSharp regex-redux

run:
	/usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v /usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/bin/dotnet ./bin/Release/netcoreapp1.1/tmp.dll 0 < ../../regexredux-input5000000.txt
