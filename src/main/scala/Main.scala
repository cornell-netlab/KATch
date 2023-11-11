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
    if Options.convertToKat then appendToRunFreneticScript(s"echo \"Running frenetic dump bisim on $file\"\n")
    Runner.runTopLevel(file.toString)
  }
  // Append msg to benchresults.txt
  val fw = new FileWriter("benchresults/benchresults.txt", true) // true to append
  fw.write("\n")
  fw.close()

def deleteDirectory(path: Path): Unit = {
  // Recursive deletion of files in the directory
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
}

def appendToRunFreneticScript(msg: String) =
  val fw = new java.io.FileWriter(s"kat/runfrenetic.sh", true) // true to append
  fw.write(msg)
  fw.close()

def prepCheckFreneticScript() =
  // Delete the `kat` dir and its contents and recreate it
  val katDir = Paths.get("kat")
  deleteDirectory(katDir)
  Files.createDirectory(katDir)
  // Write the check function to kat/runfrenetic.sh
  val fw = new FileWriter("kat/runfrenetic.sh", false) // false to overwrite
  fw.write("""
#!/bin/bash

check_result() {
    if [ $1 == $2 ]; then
        printf "\e[32msuccess for $1, $2 - expected: $expected_output (time: "
    else
        # print failure message in red to stderr along with time
        printf "\e[31mfailure for $1, $2 - expected: $expected_output, got: $output (time: "
    fi
}

check_bisim() {

    # run frenetic dump bisim and capture the output and time
    local output
    ( time frenetic dump bisim $1 $2 | (read output; check_result $output $3)  | tee /dev/tty ) 2>&1 > /dev/null  | grep real | sed 's/real//' | tr -d '\n'
    printf ")\e[0m\n"
}

""")
  fw.close()

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
    case "run" =>
      println("Running NKPL:")
      Options.convertToKat = false
      runFilesAndDirs(inputs.toList)
    case "bench" =>
      println("Benchmarking:")
      for (i <- 0 to 10) runFilesAndDirs(List("benchmarks/large"))
    case _ => error(s"Invalid command $command")
  }
