# Energy Efficiency in Programming Languages
#### Checking Energy Consumption in Programming Languages Using the _Computer Language Benchmark Game_ as a case study.

### What is this?

This repo contains the ...

It supports, for each benchmark of each language, 4 operations: *(1)* **compilation**, *(2)* **execution**, *(3)* **energy measuring** and *(4)* **memory peak detection**.

### How is it structured and hows does it work?

This framework follows a specific folder structure, which guarantees the correct workflow when the goal is to perform and operation for all benchmarks at once.
Moreover, it must be defined, for each benchmark, how to perform the 4 operations considered.

Next, we explain the folder structure and how to specify, for each language benchmark, the execution of each operation.

#### The Structure
The main folder contains 31 elements: 
1. 29 sub-folders (one for each of the considered languages); each folder contains a sub-folder for each considered benchmark.
2. A `Python` script `compile_all.py`, capable of building, running and measuring the energy and memory usage of every benchmark in all considered languages.
2. A `RAPL` sub-folder, containing the code of the energy measurement framework.

Basically, the directories tree will look something like this:

```Java
| ...
| <Language-1>
	| <benchmark-1>
		| <source>
		| Makefile
		| [input]
	| ...
	| <benchmark-i>
		| <source>
		| Makefile
		| [input]
| ...
| <Language-i>
	| <benchmark-1>
	| ...
	| <benchmark-i>
| RAPL
| compile_all.py

```

Taking the `C` language as an example, this is how the folder for the `binary-trees` and `k-nucleotide` benchmarks would look like:

```Java
| ...
| C
	| binary-trees
		| binarytrees.gcc-3.c
		| Makefile
	| k-nucleotide
		| knucleotide.c
		| knucleotide-input25000000.txt
		| Makefile
	| ...
| ...

```

#### The Operations

Each benchmark sub-folder, included in a language folder, contains a `Makefile`.
This is the file where is stated how to perform the 4 supported operations: *(1)* **compilation**, *(2)* **execution**, *(3)* **energy measuring** and *(4)* **memory peak detection**.

Basically, each `Makefile` **must** contains 4 rules, one for each operations:

| Rule | Description |
| -------- | -------- |
| `compile` | This rule specifies how the benchmark should be compiled in the considered language; Interpreted languages don't need it, so it can be left blank in such cases. |
| `run` | This rule specifies how the benchmark should be executed; It is used to test whether the benchmark runs with no errors, and the output is the expected. |
| `measure` | This rule shows how to use the framework included in the `RAPL` folder to measure the energy of executing the task specified in the `run` rule. |
| `mem` | Similar to `measure`, this rule executes the task specified in the `run` rule but with support for memory peak detection. |

To better understand it, here's the `Makefile` for the `binary-trees` benchmark:

```Makefile
compile:
	/usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -fopenmp -D_FILE_OFFSET_BITS=64 -I/usr/include/apr-1.0 binarytrees.gcc-3.c -o binarytrees.gcc-3.gcc_run -lapr-1 -lgomp -lm
	
measure:
	sudo ../../RAPL/main "./binarytrees.gcc-3.gcc_run 21" C binary-trees

run:
	./binarytrees.gcc-3.gcc_run 21

mem:
	/usr/bin/time -v ./binarytrees.gcc-3.gcc_run 21

```

### Running an example.

We included a main Python script, `compile_all.py`, that you can either call from the main folder or from inside a language folder, and it can be executed as follows:

```Bash
python compile_all.py [rule]
```

You can provide a rule from the available 4 referenced before, and the script will perform it using **every** `Makefile` found in the same folder level and bellow.

The default rule is `compile`, which means that if you run it with no arguments provided (`python compile_all.py`) the script will try to compile all benchmarks.

The results of the energy measurements will be stored in files with the name `<language>.csv`, where `<language>` is the name of the running language. 
You will find such file inside of corresponding language folder.

### Add your own example!
#### Wanna know your own code's energy behavior? We help you!
#### Follow this steps:

##### 1. Create a folder with the name of you benchmark, such as `test-benchmark`, inside the language you implemented it.

##### 2. Follow the instructions presented in the [Operations](#the-operations) section, and fill the `Makefile`.

##### 3. Use the `compile_all.py` script to compile, run, and/or measure what you want! Or run it yourself using the [`make`](https://linux.die.net/man/1/make) command.

### Further Reading
Wanna know more? Check [this website](https://sites.google.com/view/energy-efficiency-languages)!

There you can find the results of a successful experimental setup using the contents of this repo, and the used machine and compilers specifications.

You can also find there the paper which include such results and our discussion on them:

>**"_Energy Efficiency across Programming Languages: How does Energy, Time and Memory Relate?_"**, 
>Rui Pereira, Marco Couto, Francisco Ribeiro, Rui Rua, Jácome Cunha, João Paulo Fernandes, and João Saraiva. 
>In *Proceedings of the 10th International Conference on Software Language Engineering (SLE '17)*

### Contacts

[Green Software Lab](http://greenlab.di.uminho.pt)

Main contributors: [@Marco Couto](http://github.com/MarcoCouto) and [@Rui Pereira](http://github.com/States)
