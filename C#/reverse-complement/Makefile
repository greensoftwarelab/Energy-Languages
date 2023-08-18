ifndef DOTNET
	DOTNET := dotnet
endif

compile:
	$(DOTNET) new console -o build -n build --force
	cp revcomp.cs build/Program.cs
	$(DOTNET) build build/ -c Release --no-incremental

run:
	$(DOTNET) run -c Release --project build/ --no-build -- 0 < ../../revcomp-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl $(JSON) $(DOTNET) run -c Release --project build/ --no-build -- 0 < ../../revcomp-input25000000.txt
