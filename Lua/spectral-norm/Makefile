compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(LUA) spectralnorm.lua 5500" Lua spectral-norm

run:
	$(LUA) spectralnorm.lua 5500

mem:
	/usr/bin/time -v $(LUA) spectralnorm.lua 5500

valgrind:
	valgrind --tool=massif --stacks=yes $(LUA) spectralnorm.lua 5500
