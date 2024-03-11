# Artifact for "A Fast Symbolic Verifier for NetKAT"

*NetKAT* is a regular-expression-like language for specifying the trace behavior of networks. Network verification queries can be expressed as equivalence or containment queries between NetKAT policies. We present *KATch*, which implements a DSL for for NetKAT verification queries called *NKPL* (NetKAT programming language).

The following NKPL script verifies the equivalence of two NetKAT policies:
```
-- Define two simple NetKAT policies
net1 = (((@a←1 ⋅ @b←2 ⋅ @c←3 ⋅ δ)⋆) + ((@b=2 ⋅ @c=3 ⋅ δ)⋆))⋆
net2 = (((@b=2 ⋅ @c=3 ⋅ δ)⋆) + ((@a←1 ⋅ @b←2 ⋅ @c←3 ⋅ δ)⋆))⋆

-- Verify that net1 is equivalent to net2
check net1 ≡ net2

-- Visualise the policy in the given output directory
graphviz "out/net1" net1
```
A tutorial for NKPL is available in the `nkpl/tutorial.nkpl` file.

## Quick start guide

You can install and run KATch using Docker:

1. Install [Docker](https://www.docker.com/get-started/).
2. Run `docker run -it -v $PWD/results:/katch/results julesjacobs/katch:latest` on your command line. You are now inside the KATch Docker image, but the results directory will exist *outside* of the Docker (in the current directory) also.
3. Run `./katch run nkpl/tutorial.nkpl` to run KATch on the tutorial NKPL file.
4. View the resulting graphviz visualisation and running time in your local `results` directory, which was bound to the `/katch/results` directory in the Docker image in step 2.

## Evaluation instructions

To evaluate this artifact, you can follow these steps:

### 1. Learn about NKPL

Read the `tutorial.nkpl` file to learn about the NKPL language and how to write NKPL programs.

### 2. Write your own NKPL programs

Write your own NKPL programs to verify NetKAT policies. You can use the `tutorial.nkpl` file as a starting point.

### 3. Run the tests

Run the tests in the `nkpl/tests` directory to verify the correctness of KATch. You can use the following command to run all `.nkpl` files in the `nkpl/tests` directory:

```
./katch run nkpl/tests
```

 (These include 500,000 tests generated by fuzzing; takes ~15min)

### 4. Reproduce Benchmarks from the Paper

We have provided several additional scripts to run all of the experiments in the paper and generate the associated graphs, including comparisons with [Frenetic](https://github.com/frenetic-lang/frenetic). The estimates for running time are on a machine running Ubuntu 22.04 with a 2.1GHz Xeon Silver 4216 CPU and 500G RAM. The scripts are:

- `scripts/paper-exper.sh`: Runs all of the experiments from the paper and generates Figure 9, Figure 10, and Figure 11. This takes between 1-2 days.
- `scripts/abridged.sh`: Runs all of the experiments from the paper, *except* that a 1 minute timeout is used for Figure 9 (instead of 5 minute), and the Cogentco benchmarks from Figure 10 are omitted. This takes 12-15 hours.
- `scripts/katch-only.sh`: Runs all of the experiments from the paper with no timeouts, but only runs KATch (not Frenetic). This takes 1.5 hours.

Note that for the experiments plots to make sense, at most one of `paper-exper.sh` and `abridged.sh` should be run. One can reset and run the other by deleting `plots/comparison.csv` (or using a fresh instance of the docker image).

### 5. Analyse the results

Analyse the results of the benchmarks in the `results` directory to evaluate the performance of KATch.
The above scripts will reproduce the plots from Figures 9, 10, and 11 in the paper and place them in the `results/plots` directory. The `results` directory will also contain the raw data from the experiments (`results/comparison.csv`), as well as the graphviz visualisations of the NetKAT policies you have run (remaining subdirectories).

## File structure

This artifact contains the source code for KATch, as well as a suite of benchmarks and scripts for running the benchmarks. You can also find the [entire source code (and Dockerfile) on GitHub](https://github.com/julesjacobs/KATch/tree/master).
This artifact is organised as follows:

- `src/main/scala`: Source code for KATch
  - `Main.scala`: Main entry point for KATch
  - `Runner.scala`: Implementation of the runner for NKPL
  - `NK.scala`: Implementation of the NetKAT AST
  - `Parser.scala`: Implementation of the parser for NKPL
  - `SPP.scala`: Implementation of the symbolic policy representation
  - `Bisim.scala`: Implementation of the bisimulation algorithm
  - `Options.scala`: Configuration options for KATch
  - `Viz.scala`: Implementation of the visualisation tool for NetKAT policies
  - `BenchGen.worksheet.sc`: Implementation of the benchmark generator
  - `Fuzzer.worksheet.sc`: Implementation of the fuzzer for SPs and SPPs
- `nkpl`: NKPL files
  - `tutorial.nkpl`: Tutorial for NKPL
  - `tests`: Test cases for NKPL
  - `fig09`: Benchmarks for Figure 9
  - `fig10`: Benchmarks for Figure 10
  - `fig10-less-cogentco`: Benchmarks for Figure 10, excluding Cogentco (the largest benchmark)
  - `fig11`: Benchmarks for Figure 11
- `results`: Results of the benchmarks and graphviz visualisations
- `scripts`: Various scripts to aid in running KATch and comparing to Frenetic
- `Dockerfile`: The Dockerfile can be used to build the Docker image for KATch
