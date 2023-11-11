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

@main def hello(inputs: String*): Unit =
  if inputs.isEmpty then println("No directories or files specified, ")
  val dirsAndFiles = if inputs(0) == "convert" then
    Options.convertToKat = true
    println("Converting to Kat")
    inputs.tail
  else
    println("Running NKPL")
    inputs
  val nkplFiles = dirsAndFiles.flatMap { dirOrFile =>
    if dirOrFile.endsWith(".nkpl") then List(Paths.get(dirOrFile))
    else findFiles(Paths.get(dirOrFile), ".nkpl")
  } sortBy (_.getFileName.toString)
  println(nkplFiles)

  for (file <- nkplFiles) {
    Runner.runTopLevel(file.toString)
  }
  // Append msg to benchresults.txt
  val fw = new FileWriter("benchresults/benchresults.txt", true) // true to append
  fw.write("\n")
  fw.close()
