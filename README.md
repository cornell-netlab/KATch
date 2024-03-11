# Artifact for "A Fast Symbolic Verifier for NetKAT"

*NetKAT* is a regular-expression-like language for specifying the trace behavior of networks. Network verification queries can be expressed as equivalence or containment queries between NetKAT policies. We present *KATch*, a fast symbolic verifier for NetKAT, which you can use via a high-level DSL called *NKPL* (NetKAT programming language):

```
-- Define two simple NetKAT policies
net1 = (((@a←1 ⋅ @b←2 ⋅ @c←3 ⋅ δ)⋆) + ((@b=2 ⋅ @c=3 ⋅ δ)⋆))⋆
net2 = (((@b=2 ⋅ @c=3 ⋅ δ)⋆) + ((@a←1 ⋅ @b←2 ⋅ @c←3 ⋅ δ)⋆))⋆

-- Verify that net1 is equivalent to net2
check net1 ≡ net2

-- Visualise the policy
graphviz "out/net1" net1
```
A tutorial for NKPL is available in the `nkpl/tutorial.nkpl` file.

## Quick start guide

You can install and run KATch using Docker:

1. Install [Docker](https://www.docker.com/get-started/).
2. Run `docker run -it julesjacobs/katch:latest` on your command line. You are now inside the KATch Docker image.
3. Run `./katch run nkpl/tutorial.nkpl` to run KATch on the tutorial NKPL file.
4. Run `./katch run nkpl/tests` to run all the tests in the `nkpl/tests` directory.

## Evaluation instructions

To evaluate this artifact, you can follow these steps:

### 1. Learn about NKPL

Read the `tutorial.nkpl` file to learn about the NKPL language and how to write NKPL programs.

### 2. Write your own NKPL programs

Write your own NKPL programs to verify NetKAT policies. You can use the `tutorial.nkpl` file as a starting point.


### 3. Run the tests

Run the tests in the `nkpl/tests` directory to verify the correctness of KATch. You can use the `./katch run nkpl/tests` command to run the tests.

### 4. Reproduce Benchmarks from the Paper

We have provided several additional scripts to run all of the experiments in the paper and generate the associated graphs. The estimates for running time are on a machine running Ubuntu 22.04 with a 2.1GHz Xeon Silver 4216 CPU and 500G RAM. The scripts are:

- `scripts/paper-exper.sh`: Runs all of the experiments from the paper and generates Figure 9, Figure 10, and Figure 11. This takes between 1-2 days.
- `scripts/abridged.sh`: Runs all of the experiments from the paper, *except* the Cogentco benchmarks from Figure 11, and a 1 minute timeout is used for Figure 9. This takes 10 hours.
- `scripts/katch-only.sh`: Runs all of the experiments from the paper with no timeouts, but only runs KATch (not Frenetic). This takes 1.5 hours.

### 5. Analyse the results

Analyse the results of the benchmarks in the `benchresults` directory to evaluate the performance of KATch.

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