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

def prepCheckFreneticScript() =
  // Delete the `kat` dir and its contents and recreate it
  val katDir = Paths.get("kat")
  deleteDirectory(katDir)
  Files.createDirectory(katDir)
  // Write the check function to kat/runfrenetic.sh
  val fw = new FileWriter("kat/runfrenetic.sh", false) // false to overwrite
  fw.write("""
#!/bin/bash

check_bisim() {
    # Assign arguments to variables
    local file1=$1
    local file2=$2
    local expected_output=$3

    # Run frenetic dump bisim and capture the output and time
    local output
    local time_info
    time_info=$( { time frenetic dump bisim "$file1" "$file2" 2>&1 1>&3 3>&- | sed 's/real//'; } 3>&1 )

    # Compare the output with the expected output and print a single line result
    if [ "$output" == "$expected_output" ]; then
        # Print success message in green along with time
        echo -e "\e[32mSuccess for $file1, $file2 - Expected: $expected_output (Time: $time_info)\e[0m"
    else
        # Print failure message in red to stderr along with time
        echo -e "\e[31mFailure for $file1, $file2 - Expected: $expected_output, Got: $output (Time: $time_info)\e[0m" >&2
    fi
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
      for (i <- 0 to 5) runFilesAndDirs(List("benchmarks/large"))
    case _ => error(s"Invalid command $command")
  }
