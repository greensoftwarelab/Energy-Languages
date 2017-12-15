compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/lua-5.3.3/bin/lua  knucleotide.lua-2.lua 0 < knucleotide-input25000000.txt" Lua k-nucleotide

run:
	/usr/local/src/lua-5.3.3/bin/lua  knucleotide.lua-2.lua 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/lua-5.3.3/bin/lua  knucleotide.lua-2.lua 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/lua-5.3.3/bin/lua  knucleotide.lua-2.lua 0 < knucleotide-input25000000.txt
