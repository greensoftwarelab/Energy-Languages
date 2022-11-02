compile:
	cp pidigits.jruby pidigits.rb

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m pidigits.rb 10000" JRuby pidigits

run:
	$(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m pidigits.rb 10000

mem:
	/usr/bin/time -v $(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m pidigits.rb 10000

valgrind:
	valgrind --tool=massif --stacks=yes $(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m pidigits.rb 10000 
