compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/lua-5.3.3/bin/lua  nbody.lua-2.lua 50000000" Lua n-body

run:
	/usr/local/src/lua-5.3.3/bin/lua  nbody.lua-2.lua 50000000

mem:
	/usr/bin/time -v  /usr/local/src/lua-5.3.3/bin/lua  nbody.lua-2.lua 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/lua-5.3.3/bin/lua  nbody.lua-2.lua 50000000
