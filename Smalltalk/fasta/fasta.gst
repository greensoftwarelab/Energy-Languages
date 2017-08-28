"* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

Object subclass: #RandomNumber
   instanceVariableNames: 'seed scale'
   classVariableNames: 'FModulus Increment Modulus Multiplier'
   poolDictionaries: ''
   category: 'Shootout'!

ReadStream subclass: #RepeatStream
   instanceVariableNames: 'repeatPtr repeatLimit'
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Shootout'!

RepeatStream subclass: #RandomStream
   instanceVariableNames: 'random percentages'
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Shootout'!

!RandomNumber methodsFor: 'private'!
to: anInteger
   seed := 42.
   scale := anInteger! !

!RandomNumber methodsFor: 'accessing'!
next
   seed := (seed * Multiplier + Increment) \\ Modulus.
   ^(seed * scale) / FModulus! !


!RandomNumber class methodsFor: 'class initialization'!
initialize
   FModulus := 139968.0d0.
   Increment := 29573.
   Modulus := 139968.
   Multiplier := 3877.! !

!RandomNumber class methodsFor: 'initialize-release'!
to: anInteger
   ^self basicNew to: anInteger! !


!RepeatStream methodsFor: 'accessing'!
next
   position >= readLimit ifTrue: [ self position: 0 ].
   repeatPtr := repeatPtr + 1.
   ^collection at: (position := position + 1)! !

!RepeatStream methodsFor: 'testing'!
atEnd
   ^repeatPtr >= repeatLimit! !

!RepeatStream methodsFor: 'initialize-release'!
to: anInteger
   repeatPtr := 0.
   repeatLimit := anInteger! !


!RandomStream methodsFor: 'accessing'!
next
   | r |
   r := random next.
   repeatPtr := repeatPtr + 1.
   1 to: percentages size do: [:i|
      (r < (percentages at: i)) ifTrue: [^collection at: i]]! !

!RandomStream methodsFor: 'accessing'!
random: aRandomNumber
"* Share the random number generator so we can get the expected results. *"
   random := aRandomNumber! !

!RandomStream methodsFor: 'initialize-release'!
on: aCollection
   | size cp |
   repeatPtr := 0.
   random := RandomNumber to: 1.0d0.
   size := aCollection size.
   percentages := Array new: size.
   collection := Array new: size.
   cp := 0.0d0.
   1 to: size do: [:i|
      collection at: i put: (aCollection at: i) first.
      percentages at: i put: (cp := cp + (aCollection at: i) last).
   ]! !


!RepeatStream class methodsFor: 'instance creation'!
to: anInteger on: aCollection
   ^(super on: aCollection) to: anInteger! !


!Tests class methodsFor: 'benchmarking'!
writeFasta: aString from: inStream to: outStream lineLength: lineLength
   | i |
   outStream nextPut: $>; nextPutAll: aString; nl.
   i := 0.
   [inStream atEnd] whileFalse:
      [i == lineLength ifTrue: [outStream nl. i := 0].
      outStream nextPut: inStream next.
      i := i + 1].
   outStream nl! !

!Tests class methodsFor: 'benchmarking'!
fasta: n to: out
   | r lineLength |
   lineLength := 60.
   self
      writeFasta: 'ONE Homo sapiens alu'
      from:
         ( RepeatStream
            to: n*2
            on:'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG',
               'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA',
               'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT',
               'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA',
               'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG',
               'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC',
               'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA' )
      to: out
      lineLength: lineLength.

   r := RandomNumber to: 1. "Shared random sequence"

   self
      writeFasta: 'TWO IUB ambiguity codes'
      from:
         (( RandomStream
            to: n*3
            on: #(   #($a 0.27d0)
                  #($c 0.12d0)
                  #($g 0.12d0)
                  #($t 0.27d0)

                  #($B 0.02d0)
                  #($D 0.02d0)
                  #($H 0.02d0)
                  #($K 0.02d0)
                  #($M 0.02d0)
                  #($N 0.02d0)
                  #($R 0.02d0)
                  #($S 0.02d0)
                  #($V 0.02d0)
                  #($W 0.02d0)
                  #($Y 0.02d0)))
         random: r;
         yourself)
      to: out
      lineLength: lineLength.

   self
      writeFasta: 'THREE Homo sapiens frequency'
      from:
         (( RandomStream
            to: n*5
            on: #(   #($a 0.3029549426680d0)
                  #($c 0.1979883004921d0)
                  #($g 0.1975473066391d0)
                  #($t 0.3015094502008d0)))
            random: r;
            yourself)
      to: out
      lineLength: lineLength.

   out flush. ! !


!Tests class methodsFor: 'benchmark scripts'!
fasta
   self fasta: self arg to: self stdoutSpecial.
   ^''! !


RandomNumber initialize!