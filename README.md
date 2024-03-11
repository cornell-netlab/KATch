# Artifact for "A Fast Symbolic Verifier for NetKAT"

NetKAT is a language for programming and verifying software-defined networks. It is based on Kleene Algebra with Tests (KAT), a formalism for reasoning about programs with loops and conditionals. NetKAT is a high-level language for programming network policies, and it is designed to be amenable to formal verification. In this paper, we present KATch, a fast symbolic verifier for NetKAT. KATch is based on a novel symbolic representation of NetKAT policies, and it is designed to be fast and scalable. We have implemented KATch and evaluated it on a suite of benchmarks. Our results show that KATch is significantly faster than existing tools for verifying NetKAT policies.

We have designed a high-level DSL called NKPL (NetKAT programming language) for NetKAT verification. The expression language is based on the NetKAT language, whereas the statement language contains constructs for defining and verifying properties of NetKAT policies. A tutorial for NKPL is available in the `nkpl/tutorial.nkpl` file.

## Quick start guide

You can install and run KATch using Docker:

1. Install [Docker](https://www.docker.com/get-started/).
2. Run `docker run -it julesjacobs/katch:latest` on your command line. You are now inside the KATch Docker image.

To run KATch on an NKPL source file, you can use the `./katch run <file>` command, where `<file>` is the path to the NKPL file you want to verify. For example, to run the `tutorial.nkpl` file, you can use the following command:

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

## Evaluation instructions

To evaluate this artifact, you can follow these steps:

1. Learn about NKPL: Read the `tutorial.nkpl` file to learn about the NKPL language and how to write NKPL programs.
2. Write your own NKPL programs: Write your own NKPL programs to verify NetKAT policies. You can use the `tutorial.nkpl` file as a starting point.
3. Run the tests: Run the tests in the `nkpl/tests` directory to verify the correctness of KATch. You can use the `./katch run nkpl/tests` command to run the tests.
4. Run the benchmarks: Run the benchmarks in the `nkpl/benchmarks` directory to evaluate the performance of KATch. You can use the `./katch run` command to run the benchmarks.
5. Analyse the results: Analyse the results of the benchmarks in the `benchresults` directory to evaluate the performance of KATch.

## Reproduce Experiments from the Paper

We have provided several additional scripts to run all of the experiments in the paper and generate the associated graphs. The estimates for running time are on a machine running Ubuntu 22.04 with a 2.1GHz Xeon Silver 4216 CPU and 500G RAM. The scripts are:

- `scripts/paper-exper.sh`: Runs all of the experiments from the paper and generates Figure 9, Figure 10, and Figure 11. This takes between 1-2 days.
- `scripts/abridged.sh`: Runs all of the experiments from the paper, *except* the Cogentco benchmarks from Figure 11, and a 1 minute timeout is used for Figure 9. This takes 10 hours.
- `scripts/katch-only.sh`: Runs all of the experiments from the paper with no timeouts, but only runs KATch (not Frenetic). This takes 1.5 hours.


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
- `scripts`: Various scripts to aid in running KATch and comparing to Frenetic
- `Dockerfile`: The Dockerfile can be used to build the Docker image for KATch