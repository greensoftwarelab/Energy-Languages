#!/bin/bash

echo "Generating input for k-nucleotide/reverse-complement"
python3 Python/fasta/fasta.py 25000000 > knucleotide-input25000000.txt
cp knucleotide-input25000000.txt revcomp-input25000000.txt

echo "Generating input for regex-redux"
python3 Python/fasta/fasta.py 5000000 > regexredux-input5000000.txt
