# java -Xmx2g -Xss35m -XX:MaxInlineLevel=9 -jar target/scala-3.3.1/KATch-assembly-0.1.0-SNAPSHOT.jar bench

# native-image --pgo-instrument -jar target/scala-3.3.1/KATch-assembly-0.1.0-SNAPSHOT.jar
# ./KATch-assembly-0.1.0-SNAPSHOT bench
# native-image --pgo=default.iprof -jar target/scala-3.3.1/KATch-assembly-0.1.0-SNAPSHOT.jar

java -jar target/scala-3.3.1/KATch-assembly-0.1.0-SNAPSHOT.jar bench