compile:

	cp fannkuchredux.jruby-2.jruby fannkuchredux.rb

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/jruby-9.1.7.0/bin/jruby -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m fannkuchredux.rb 12" JRuby fannkuch-redux

run:
	/usr/local/src/jruby-9.1.7.0/bin/jruby -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m fannkuchredux.rb 12

mem:
	/usr/bin/time -v /usr/local/src/jruby-9.1.7.0/bin/jruby -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m fannkuchredux.rb 12

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/jruby-9.1.7.0/bin/jruby -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m fannkuchredux.rb 12
