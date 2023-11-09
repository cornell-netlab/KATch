import nkpl._
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import java.io.FileWriter

def findFiles(startDir: Path, extension: String): List[Path] = {
  Files
    .walk(startDir)
    .iterator()
    .asScala
    .filter(path => !Files.isDirectory(path) && path.toString.endsWith(extension))
    .toList
}

@main def hello: Unit =
  val directories = List(
    // "benchmarks/tiny",
    // "benchmarks/small",
    // "benchmarks/medium",
    // "benchmarks/large",
    "nkpl"
  )
  val nkplFiles = directories.flatMap(dir => findFiles(Paths.get(dir), ".nkpl")).sortBy(_.getFileName.toString)
  // val nkplFiles = List("benchmarks/large/Telcove_slicing.nkpl")
  // val nkplFiles = List("benchmarks/large/ft8_reachability.nkpl")
  // .filter(_.toString.endsWith(".nkpl"))
  // .toArray
  // .map(_.asInstanceOf[Path])
  // .toSeq

  for (i <- 1 to 5)
    for (file <- nkplFiles) {
      Runner.runTopLevel(file.toString)
    }
    // Runner.runTopLevel("benchmarks/tiny/t4_near_reachability.nkpl")
    // Runner.runTopLevel("benchmarks/tiny/t4_slicing.nkpl")
    // Runner.runTopLevel("nkpl/test.nkpl")

    // Append msg to benchresults.txt
    val fw = new FileWriter("benchresults/benchresults.txt", true) // true to append
    try {
      fw.write("\n")
    } finally {
      fw.close()
    }
