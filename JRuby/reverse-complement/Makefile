compile:
	cp revcomp.jruby revcomp.rb

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m revcomp.rb 0 < ../../revcomp-input25000000.txt" JRuby reverse-complement

run:
	$(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m revcomp.rb 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v $(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m revcomp.rb 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(JRUBY) -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m revcomp.rb 0 < ../../revcomp-input25000000.txt
