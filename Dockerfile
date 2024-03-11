# Use an official Java runtime as a parent image
FROM openjdk:8

# Set the working directory inside the container
WORKDIR /katch

# Install sbt
RUN apt-get update && \
    apt-get install -y apt-transport-https curl gnupg && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add && \
    apt-get update && \
    apt-get install -y sbt opam

# Copy the relevant contents into the container at /katch
COPY nkpl/tutorial.nkpl nkpl/tutorial.nkpl
COPY nkpl/fig09 nkpl/fig09
COPY nkpl/fig10 nkpl/fig10
COPY nkpl/fig11 nkpl/fig11
COPY nkpl/fig10-less-cogentco nkpl/fig10-less-cogentco
COPY nkpl/networks nkpl/networks
COPY nkpl/tests nkpl/tests
COPY scripts scripts
COPY src src
COPY project project
COPY build.sbt build.sbt
COPY katch katch

# Make benchresults/benchresults.txt
RUN mkdir benchresults
RUN touch benchresults/benchresults.txt

# Compile KATch and make a fat jar
RUN sbt compile
RUN sbt assembly

# Get Frenetic for comparison
RUN git clone https://www.github.com/frenetic-lang/frenetic.git
WORKDIR /katch/frenetic
RUN git checkout guarded

RUN opam init --disable-sandboxing && opam switch create 4.13.0
RUN opam install --deps-only -y .
RUN opam install -y dune

# In lieu of `eval $(opam env)`:
ENV OPAM_SWITCH_PREFIX='/root/.opam/4.13.0'
ENV CAML_LD_LIBRARY_PATH='/root/.opam/4.13.0/lib/stublibs:/root/.opam/4.13.0/lib/ocaml/stublibs:/root/.opam/4.13.0/lib/ocaml'
ENV OCAML_TOPLEVEL_PATH='/root/.opam/4.13.0/lib/toplevel'
ENV MANPATH=':/root/.opam/4.13.0/man'
ENV PATH='/root/.opam/4.13.0/bin:/usr/local/openjdk-8/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin'

RUN dune build || printf "ok\n"
RUN dune install
WORKDIR /katch

# Install dependencies to generate plots
RUN apt-get install -y python3-pip
RUN pip install matplotlib seaborn jinja2
RUN mkdir plots

# Put the user in the shell
# They have to run the image with the -it flag: `docker run -it katch`
CMD ["/bin/bash"]
