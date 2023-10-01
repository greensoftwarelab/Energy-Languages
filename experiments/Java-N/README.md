# Goals of Java-N
- Measure in-process iterations of each benchmark to observe increase in performance over program runtime.
- Observe differences between benchmarks due to base runtime, memory overhead, etc.

# Changes made from the original Java benchmarks
- All benchmarks
    - The bulk of computation (everything excluding imports, global variable declarations) in every benchmark was wrapped in a for loop with a variable specified number of maximum iterations "NNNNN".
    ```
    public static void main(String[] args) {
        // NNNNN gets replaced at build time by a constant.
        for (int i = 0; i < NNNNN; ++i) {
            // Original benchmark source.
        }
    }
    ```
    - Then each benchmark was converted to ".java.in", which are not runnable, but when the "make compile" command is run during compilation done by the "measure.py" script, the file is converted back into ".java" and the "NNNNN" variable is set to a specified integer.
- regex-redux, reverse-complement
    - Uses of System.in.read were replaced with FileInputStream readers due to the fact that the buffer would run out after one iteration.
    - For reverse-complement, the Finder class now passes in a ExecutorService object as an argument.
