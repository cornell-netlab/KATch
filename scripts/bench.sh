# This runs the profiler on the benchmarking program
# The benchmarking program is run by `sbt run`, but this compiles first so we need to wait 5 seconds before starting the profiler.
# The profiler should be run via ./aprof/profiler.sh -d 30 -f flamegraph.html 15253
# However, instead of the last argument being 15253, it should be the PID of the sbt process.
# We can find that using jps, which will have output like this:
#   15253 sbt-launch.jar
#   2422 Main
#   15336 Jps
#   87054 Server

# Now we implement this in bash:
# 1. Run sbt in the background
# 2. Wait 2 seconds
# 3. Run jps to get the PID of the sbt process
# 4. Run the profiler on the sbt process

MEM=$1
if [ $# -ne 1 ]; then
  # Set memory to 4GB by default
  MEM="4g"
fi

sbt assembly
# Here is the code:
java -Xmx"$MEM" -XX:MaxInlineLevel=20 -Xss35m -jar target/scala-3.3.1/KATch-assembly-0.1.0-SNAPSHOT.jar bench &
sleep 0.01
PID=$(jps | grep SNAPSHOT.jar | cut -d ' ' -f 1)
./aprof/profiler.sh -d 60 -f flamegraph.html $PID
