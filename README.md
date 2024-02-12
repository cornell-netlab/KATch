# Artifact for "KATch: A Fast Symbolic Verifier for NetKAT"

NetKAT is a language for programming and verifying software-defined networks. It is based on Kleene Algebra with Tests (KAT), a formalism for reasoning about programs with loops and conditionals. NetKAT is a high-level language for programming network policies, and it is designed to be amenable to formal verification. In this paper, we present KATch, a fast symbolic verifier for NetKAT. KATch is based on a novel symbolic representation of NetKAT policies, and it is designed to be fast and scalable. We have implemented KATch and evaluated it on a suite of benchmarks. Our results show that KATch is significantly faster than existing tools for verifying NetKAT policies.

We have designed a high-level DSL called NKPL (NetKAT programming language) for NetKAT verification. The expression language is based on the NetKAT language, whereas the statement language contains constructs for defining and verifying properties of NetKAT policies. A tutorial for NKPL is available in the `nkpl/tutorial.nkpl` file.


## File structure

This artifact contains the source code for KATch, as well as a suite of benchmarks and scripts for running the benchmarks.
This artifact is organised as follows:

- `src/main/scala`: Source code for KATch
  - `Main.scala`: Main entry point for KATch
  - `Runner.scala`: Implementation of the runner for NKPL
  - `NK.scala`: Implementation of the NetKAT AST
  - `Parser.scala`: Implementation of the parser for NKPL
  - `SPP.scala`: Implementation of the symbolic policy representation
  - `Bisim.scala`: Implementation of the bisimulation algorithm
  - `Options.scala`: Configuration options for KATch
  - `BenchGen.worksheet.sc`: Implementation of the benchmark generator
  - `Fuzzer.worksheet.sc`: Implementation of the fuzzer for NetKAT policies
  - `Viz.worksheet.sc`: Implementation of the visualisation tool for NetKAT policies
- `nkpl`: NKPL files
  - `tutorial.nkpl`: Tutorial for NKPL
  - `tests`: Test cases for NKPL
  - `benchmarks`: All NKPL benchmarks used in the paper
  - `misc`: Miscellaneous NKPL files
- `benchresults`: Results of the benchmarks

## Running KATch

To run KATch, you can use the `./katch run <file>` command, where `<file>` is the path to the NKPL file you want to verify. For example, to run the `tutorial.nkpl` file, you can use the following command:

```
./katch run nkpl/tutorial.nkpl
```

This will run KATch on the `tutorial.nkpl` file and print the result to the console.

You can also run an entire directory of NKPL files using the `./katch run <dir>` command, where `<dir>` is the path to the directory containing the NKPL files you want to verify. For example, to run all the benchmarks in the `benchmarks` directory, you can use the following command:

```
./katch run nkpl/benchmarks
```

This will run KATch on all the NKPL files in the `benchmarks` directory and print the results to the console.
KATch will generate files with the results of the benchmarks in the `benchresults` directory.

## Building KATch

You can use the provided docker image to run KATch, but if you want to build KATch from source, you can follow these steps:

1. Install Scala and SBT: You can install Scala and SBT by following the instructions on the [Scala website](https://www.scala-lang.org/download/)
2. Build KATch: Run the `sbt assembly` command in the root directory of the artifact to build KATch. This will create a JAR file in the `target/` directory.
3. Run KATch: You can run KATch using the `java -jar` command, passing the path to the JAR file as an argument. Alternatively, you can use the `./katch` script in the root directory of the artifact to run KATch.

## Evaluation instructions

To evaluate this artifact, you can follow these steps:

1. Learn about NKPL: Read the `tutorial.nkpl` file to learn about the NKPL language and how to write NKPL programs.
2. Write your own NKPL programs: Write your own NKPL programs to verify NetKAT policies. You can use the `tutorial.nkpl` file as a starting point.
3. Run the tests: Run the tests in the `nkpl/tests` directory to verify the correctness of KATch. You can use the `./katch run` command to run the tests.
4. Run the benchmarks: Run the benchmarks in the `nkpl/benchmarks` directory to evaluate the performance of KATch. You can use the `./katch run` command to run the benchmarks.
5. Analyse the results: Analyse the results of the benchmarks in the `benchresults` directory to evaluate the performance of KATch.