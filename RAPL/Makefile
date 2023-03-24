

main : main.c rapl.o
	gcc -O2 -Wall -o main main.c rapl.o -lm
	chmod 777 main

rapl.o : rapl.c rapl.h
	gcc -O2 -Wall -c rapl.c  -lm

clean:
	rm -rf main.o rapl.o
