compile:

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(LUA) regexredux.lua-2.lua 0 < ../../regexredux-input50000.txt" Lua regex-redux

run:
	$(LUA) regexredux.lua-2.lua 0 < ../../regexredux-input50000.txt

mem:
	/usr/bin/time -v $(LUA) regexredux.lua-2.lua 0 < ../../regexredux-input50000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(LUA) regexredux.lua-2.lua 0 < ../../regexredux-input50000.txt
