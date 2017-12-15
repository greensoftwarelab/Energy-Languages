compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/lua-5.3.3/bin/lua  spectralnorm.lua 5500" Lua spectral-norm

run:
	/usr/local/src/lua-5.3.3/bin/lua  spectralnorm.lua 5500

mem:
	/usr/bin/time -v /usr/local/src/lua-5.3.3/bin/lua  spectralnorm.lua 5500

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/lua-5.3.3/bin/lua  spectralnorm.lua 5500
