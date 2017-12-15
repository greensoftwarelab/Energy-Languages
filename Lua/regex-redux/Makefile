compile:

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/lua-5.3.3/bin/lua  regexredux.lua-2.lua 0 < ../../regexredux-input50000.txt" Lua regex-redux

run:
	/usr/local/src/lua-5.3.3/bin/lua  regexredux.lua-2.lua 0 < ../../regexredux-input50000.txt

mem:
	/usr/bin/time -v /usr/local/src/lua-5.3.3/bin/lua  regexredux.lua-2.lua 0 < ../../regexredux-input50000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/lua-5.3.3/bin/lua  regexredux.lua-2.lua 0 < ../../regexredux-input50000.txt
