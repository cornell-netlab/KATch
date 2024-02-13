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
    apt-get install -y sbt

# Copy the relevant contents into the container at /katch
COPY nkpl/tutorial.nkpl nkpl/tutorial.nkpl
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

# Put the user in the shell
# They have to run the image with the -it flag: `docker run -it katch`
CMD ["/bin/bash"]
