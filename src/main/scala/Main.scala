import nkpl._
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import java.io.FileWriter
import java.nio.file.{Files, Paths, Path, DirectoryNotEmptyException, NoSuchFileException, FileVisitResult}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.SimpleFileVisitor
import java.io.IOException

def findFiles(startDir: Path, extension: String): List[Path] = {
  Files
    .walk(startDir)
    .iterator()
    .asScala
    .filter(path => !Files.isDirectory(path) && path.toString.endsWith(extension))
    .toList
}

def error(msg: String): Nothing = {
  println(msg)
  sys.exit(1)
}

def runFilesAndDirs(dirsAndFiles: List[String]) =
  val nkplFiles = dirsAndFiles.flatMap { dirOrFile =>
    if dirOrFile.endsWith(".nkpl") then List(Paths.get(dirOrFile))
    else findFiles(Paths.get(dirOrFile), ".nkpl")
  } sortBy (_.getFileName.toString)

  for (file <- nkplFiles) {
    Options.inputFile = file.toString
    Runner.runTopLevel(file.toString)
  }
  // Append msg to benchresults.txt
  val fw = new FileWriter("benchresults/benchresults.txt", true) // true to append
  fw.write("\n")
  fw.close()

def runFilesAndDirsFrenetic(dirsAndFiles: List[String]) =
  val nkplFiles = dirsAndFiles.flatMap { dirOrFile =>
    if dirOrFile.endsWith(".nkpl") then List(Paths.get(dirOrFile))
    else findFiles(Paths.get(dirOrFile), ".nkpl")
  } sortBy (_.getFileName.toString)

  for (file <- nkplFiles) {
    Runner.runTopLevelFrenetic(file.toString)
  }
  // Append msg to benchresults.txt
  val fw = new FileWriter("benchresults/benchresults.txt", true) // true to append
  fw.write("\n")
  fw.close()

def deleteDirectory(path: Path): Unit = {
  // Recursive deletion of files in the directory
  try {
    Files.walkFileTree(
      path,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, e: IOException): FileVisitResult = {
          if (e != null) throw e
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      }
    )
  } catch {
    case e: Exception => println(e)
  }
}

def prepCheckFreneticScript() =
  // Delete the `kat` dir and its contents and recreate it
  val katDir = Paths.get("kat")
  if Files.notExists(katDir) then Files.createDirectory(katDir)

  /*
def cleanupFreneticScript() =
  val katDir = Paths.get("kat")
  deleteDirectory(katDir)
   */

@main def hello(cmd: String*): Unit =
  if cmd.isEmpty then error("No command specified")
  val command = cmd(0)
  var inputs = cmd.tail
  command match {
    case "convert" =>
      println("Converting to KAT:")
      prepCheckFreneticScript()
      Options.convertToKat = true
      runFilesAndDirs(inputs.toList)
    case "compare" =>
      println("Comparing NKPL and KAT:")
      prepCheckFreneticScript()
      Options.convertToKat = true
      runFilesAndDirs(inputs.toList)
      Options.convertToKat = false
      runFilesAndDirs(inputs.toList)
      runFilesAndDirsFrenetic(inputs.toList)
    case "run" =>
      println("Running NKPL:")
      Options.convertToKat = false
      runFilesAndDirs(inputs.toList)
    case "bench" =>
      println("Benchmarking:")
      for (i <- 0 to 1) runFilesAndDirs(List("nkpl/scratch/bench.nkpl"))
    case _ => error(s"Invalid command $command")
  }
