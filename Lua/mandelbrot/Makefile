compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(LUA) mandelbrot.lua-6.lua 16000" Lua mandelbrot

run:
	$(LUA) mandelbrot.lua-6.lua 16000

mem:
	/usr/bin/time -v  $(LUA) mandelbrot.lua-6.lua 16000

valgrind:
	valgrind --tool=massif --stacks=yes  $(LUA) mandelbrot.lua-6.lua 16000
