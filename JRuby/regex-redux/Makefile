compile:
	cp regexredux.jruby-2.jruby regexredux.rb

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/jruby-9.1.7.0/bin/jruby -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m regexredux.rb 0 < ../../regexredux-input5000000.txt" JRuby regex-redux

run:
	/usr/local/src/jruby-9.1.7.0/bin/jruby -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m regexredux.rb 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v  /usr/local/src/jruby-9.1.7.0/bin/jruby -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m regexredux.rb 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/jruby-9.1.7.0/bin/jruby -Xcompile.fastest=true -Xcompile.invokedynamic=true -J-server -J-Xmn512m -J-Xms2048m -J-Xmx2048m regexredux.rb 0 < ../../regexredux-input5000000.txt
