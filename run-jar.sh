#!/bin/bash
sbt assembly
time java -Xmx15g -jar target/scala-3.3.1/KATch-assembly-0.1.0-SNAPSHOT.jar run $1
