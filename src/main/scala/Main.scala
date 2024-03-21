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
  // Append msg to results.txt
  val fw = new FileWriter("results/results.txt", true) // true to append
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
  // Append msg to results.txt
  val fw = new FileWriter("results/results.txt", true) // true to append
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

def init() =
  VarMap("sw")
  VarMap("pt")
  VarMap("dst")

@main def hello(cmd: String*): Unit =
  init()
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
      Options.freneticTimeout = inputs(0)
      inputs = inputs.tail
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
      for (i <- 0 to 10) runFilesAndDirs(List("nkpl/misc/scratch/bench.nkpl"))
    case "apk" =>
      println("APK benchmarks:")
      // get all files "nkpl/fig09/linear-reachability/$net.nkpl" from that directory
      val nets = findFiles(Paths.get("nkpl/fig09/linear-reachability"), ".nkpl")
        .map(_.getFileName.toString.stripSuffix(".nkpl"))
      println(nets)
      Options.supressOutput = true
      var results = Map[String, Double]()
      for (net <- nets) {
        try {
          var total_time = 0.0
          var total_runs = 0
          for (i <- 0 to 110) {
            val start = System.nanoTime()
            runFilesAndDirs(List(s"nkpl/fig09/linear-reachability/$net.nkpl"))
            val end = System.nanoTime()
            if (!(i < 10)) {
              total_time += (end - start) / 1e6
              total_runs += 1
            }
          }
          results += (net -> (total_time / total_runs))
        } catch {
          case e: Throwable => println(s"Skipping $net")
        }
      }
      for ((net, time) <- results) {
        println(s"$net & $time")
      }
      // write the same to apk_results.txt
      val fw = new FileWriter("results/apk_results.txt")
      for ((net, time) <- results) {
        fw.write(s"$net & $time\n")
      }
      fw.close()
    case _ => error(s"Invalid command $command")
  }
