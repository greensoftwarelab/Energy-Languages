compile:

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/lua-5.3.3/bin/lua  pidigits.lua-5.lua 10000" Lua pidigits

run:
	/usr/local/src/lua-5.3.3/bin/lua  pidigits.lua-5.lua 10000

mem:
	/usr/bin/time -v /usr/local/src/lua-5.3.3/bin/lua  pidigits.lua-5.lua 10000


valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/lua-5.3.3/bin/lua  pidigits.lua-5.lua 10000

