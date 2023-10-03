# Goals of C-as-C++ experiment
- Because C++ is akin to a superset of C, we decided to measure the performance of C benchmarks when compiled as C++. By changing only the method of compilation, we can observe that there is more to performance than language choice.

# Changes made from the original C benchmarks
### `chameneosredux.c`
```
93c93,95
<     int c1, c2, c3;
---
>     enum color_t                c1;
>     enum color_t                c2;
>     enum color_t                c3;
```
### Makefiles
- All output paths changed for "make measure" and input paths changed for benchmarks that require file input.

### Makefile.defaults
- CC is now set to the g++ compiler (default)
- Specifies language input with -x flag
- The -fpermissive flag is included to turn nonconformant errors into warnings, without which several benchmarks would not compile due to type conversions that are allowed in C but not in C++. To demonstrate that these compilation issues are trivial, we've included a list of them below.
```
chameneosredux.c: invalid conversion from ‘int’ to ‘color_t’
   88 |    return 0;
   99 |             c3 = color_complement(c1, c2);

knucleotide.c: invalid conversion from 'void*' to 'element'
  101 |         element * elements_Array=malloc(elements_Array_Size*sizeof(element));
  194 |         char * polynucleotide=malloc(polynucleotide_Capacity);
  206 |                         polynucleotide=realloc(polynucleotide,   polynucleotide_Capacity*=2);
  210 |         polynucleotide=realloc(polynucleotide, polynucleotide_Length);

mandelbrot.c: invalid conversion from 'void*' to 'unsigned char*'
  138 |     unsigned char * const buffer = malloc(pad + dataLength);

regexredux.c: invalid conversion from ‘void*’ to ‘char*’
   40 |             dst_String->data=realloc(dst_String->data, dst_String->capacity*=2);
   61 |         dst_String->data=realloc(dst_String->data, dst_String->capacity*=2);
   90 |     string input={malloc(16384), 16384}, sequences={malloc(16384), 16384};
  102 |             input.data=realloc(input.data, input.capacity*=2);
  132 |                 malloc(sequences.capacity), sequences.capacity, sequences.size
  134 |                 malloc(sequences.capacity), sequences.capacity

revcomp.c: invalid conversion from ‘void*’ to ‘char*’
   31 |    sequence=realloc(sequence, sequence_Size);
   76 |          char * sequence=malloc(sequence_Capacity);
   87 |               =memchr(&sequence[sequence_Size], '>', bytes_Read)); ){
  102 |                   char * const new_Sequence=malloc(READ_SIZE);
  137 |                sequence=realloc(sequence, sequence_Capacity*=2);

spectralnorm.c: invalid conversion from ‘void*’ to ‘const v2dt*’ {aka ‘const __vector(2) double*’}
   88 |     const v2dt  *pU = (void *) p->u;
  117 |     const v2dt  *pT = (void *) p->tmp;
spectralnorm.c: invalid conversion from ‘void*’ to ‘double*’
  103 |             double *mem = (void *) &sum;
  130 |             double *mem = (void *) &sum;

```
