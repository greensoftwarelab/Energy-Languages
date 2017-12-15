compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/lua-5.3.3/bin/lua  revcomp.lua-2.lua 0 < ../../revcomp-input25000000.txt" Lua reverse-complement

run:
	/usr/local/src/lua-5.3.3/bin/lua  revcomp.lua-2.lua 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/lua-5.3.3/bin/lua  revcomp.lua-2.lua 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/lua-5.3.3/bin/lua  revcomp.lua-2.lua 0 < ../../revcomp-input25000000.txt
