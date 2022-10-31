compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(LUA) revcomp.lua-2.lua 0 < ../../revcomp-input25000000.txt" Lua reverse-complement

run:
	$(LUA) revcomp.lua-2.lua 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v $(LUA) revcomp.lua-2.lua 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(LUA) revcomp.lua-2.lua 0 < ../../revcomp-input25000000.txt
