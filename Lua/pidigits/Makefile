compile:

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(LUA) pidigits.lua-5.lua 10000" Lua pidigits

run:
	$(LUA) pidigits.lua-5.lua 10000

mem:
	/usr/bin/time -v $(LUA) pidigits.lua-5.lua 10000


valgrind:
	valgrind --tool=massif --stacks=yes $(LUA) pidigits.lua-5.lua 10000

