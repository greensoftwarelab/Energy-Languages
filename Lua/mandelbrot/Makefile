compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/lua-5.3.3/bin/lua  mandelbrot.lua-6.lua 16000" Lua mandelbrot

run:
	/usr/local/src/lua-5.3.3/bin/lua  mandelbrot.lua-6.lua 16000

mem:
	/usr/bin/time -v  /usr/local/src/lua-5.3.3/bin/lua  mandelbrot.lua-6.lua 16000

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/lua-5.3.3/bin/lua  mandelbrot.lua-6.lua 16000
