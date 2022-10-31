compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(LUA) binarytrees.lua-2.lua 21" Lua binary-trees

run:
	$(LUA) binarytrees.lua-2.lua 21

mem:
	/usr/bin/time -v $(LUA) binarytrees.lua-2.lua 21

valgrind:
	valgrind --tool=massif --stacks=yes $(LUA) binarytrees.lua-2.lua 21
