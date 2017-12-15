compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/lua-5.3.3/bin/lua  fasta.lua-3.lua 25000000" Lua fasta

run:
	/usr/local/src/lua-5.3.3/bin/lua  fasta.lua-3.lua 25000000

mem:
	/usr/bin/time -v  /usr/local/src/lua-5.3.3/bin/lua  fasta.lua-3.lua 25000000

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/lua-5.3.3/bin/lua  fasta.lua-3.lua 25000000
