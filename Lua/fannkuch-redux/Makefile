compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/lua-5.3.3/bin/lua  fannkuchredux.lua 12" Lua fannkuch-redux

run:
	/usr/local/src/lua-5.3.3/bin/lua  fannkuchredux.lua 12

mem:
	/usr/bin/time -v /usr/local/src/lua-5.3.3/bin/lua  fannkuchredux.lua 12

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/lua-5.3.3/bin/lua  fannkuchredux.lua 12
