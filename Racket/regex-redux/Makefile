compile:
	$(RACO) make regexredux.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RACKET) ./compiled/regexredux_racket.zo 0 < ../../regexredux-input5000000.txt" Racket regex-redux

run:
	$(RACKET) ./compiled/regexredux_racket.zo 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v $(RACKET) ./compiled/regexredux_racket.zo 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(RACKET) ./compiled/regexredux_racket.zo 0 < ../../regexredux-input5000000.txt
