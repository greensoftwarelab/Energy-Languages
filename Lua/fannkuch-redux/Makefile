compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(LUA) fannkuchredux.lua 12" Lua fannkuch-redux

run:
	$(LUA) fannkuchredux.lua 12

mem:
	/usr/bin/time -v $(LUA) fannkuchredux.lua 12

valgrind:
	valgrind --tool=massif --stacks=yes $(LUA) fannkuchredux.lua 12
