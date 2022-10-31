compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(LUA) knucleotide.lua-2.lua 0 < knucleotide-input25000000.txt" Lua k-nucleotide

run:
	$(LUA) knucleotide.lua-2.lua 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v $(LUA) knucleotide.lua-2.lua 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(LUA) knucleotide.lua-2.lua 0 < knucleotide-input25000000.txt
