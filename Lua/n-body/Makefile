compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(LUA) nbody.lua-2.lua 50000000" Lua n-body

run:
	$(LUA) nbody.lua-2.lua 50000000

mem:
	/usr/bin/time -v  $(LUA) nbody.lua-2.lua 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  $(LUA) nbody.lua-2.lua 50000000
