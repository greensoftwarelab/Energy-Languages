compile:
	/usr/local/src/chapel-1.15.0/bin/linux64/chpl --fast pidigits.chapel-2.chpl -o pidigits.chapel-2.chapel_run
xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "./pidigits.chapel-2.chapel_run --n=10000" Chapel pidigits

run:
	./pidigits.chapel-2.chapel_run --n=10000

comp2:
	@+if [ "$(CHPL_MAKE_GMP)" != "none" ]; then \
	$(CHPL) -o $@ $(CHPL_FLAGS) $<; \
	else \
	echo "";\
	echo $(CHPL_MAKE_GMP) \
	echo "Can't build pidigits without CHPL_GMP being enabled. See for details:"; \
	echo "http://chapel.cray.com/docs/latest/usingchapel/chplenv.html#chpl-gmp"; \
	echo ""; \
	fi

mem:
	/usr/bin/time -v ./pidigits.chapel-2.chapel_run --n=10000

valgrind:
	valgrind --tool=massif --stacks=yes ./pidigits.chapel-2.chapel_run --n=10000
