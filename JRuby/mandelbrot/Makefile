compile:
	cp mandelbrot.jruby-4.jruby mandelbrot.rb

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m mandelbrot.rb 16000" JRuby mandelbrot

run:
	$(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m mandelbrot.rb 16000

mem:
	/usr/bin/time -v $(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m mandelbrot.rb 16000

valgrind:
	valgrind --tool=massif --stacks=yes $(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m mandelbrot.rb 16000
