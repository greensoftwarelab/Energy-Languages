compile:

	cp binarytrees.jruby-5.jruby binarytrees.rb

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmx2G binarytrees.rb 21" JRuby binary-trees

run:
	$(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmx2G binarytrees.rb 21

mem:
	/usr/bin/time -v $(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmx2G binarytrees.rb 21

valgrind:
	valgrind --tool=massif --stacks=yes $(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmx2G binarytrees.rb 21
