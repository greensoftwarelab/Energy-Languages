compile:
	cp nbody.jruby-2.jruby nbody.rb

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m nbody.rb 50000000" JRuby n-body

run:
	$(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m nbody.rb 50000000

mem:
	/usr/bin/time -v $(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m nbody.rb 50000000

valgrind:
	valgrind --tool=massif --stacks=yes $(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m nbody.rb 50000000
