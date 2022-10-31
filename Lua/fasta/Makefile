compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(LUA) fasta.lua-3.lua 25000000" Lua fasta

run:
	$(LUA) fasta.lua-3.lua 25000000

mem:
	/usr/bin/time -v  $(LUA) fasta.lua-3.lua 25000000

valgrind:
	valgrind --tool=massif --stacks=yes  $(LUA) fasta.lua-3.lua 25000000
