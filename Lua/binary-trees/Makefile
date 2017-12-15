compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/lua-5.3.3/bin/lua  binarytrees.lua-2.lua 21" Lua binary-trees

run:
	/usr/local/src/lua-5.3.3/bin/lua  binarytrees.lua-2.lua 21

mem:
	/usr/bin/time -v /usr/local/src/lua-5.3.3/bin/lua  binarytrees.lua-2.lua 21

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/lua-5.3.3/bin/lua  binarytrees.lua-2.lua 21
