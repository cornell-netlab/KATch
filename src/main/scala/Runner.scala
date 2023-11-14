package nkpl

import nkpl.Parser.Stmt
import nkpl.Parser.Expr

object Runner {
  type Env = Map[String, Either[NK, Int]]
  def evalNK(env: Env, v: Parser.NK): NK =
    v match {
      case Parser.Dup => Dup
      case Parser.Test(x, v) => Test(x, evalVal(env, v))
      case Parser.TestNE(x, v) => TestNE(x, evalVal(env, v))
      case Parser.Mut(x, v) => Mut(x, evalVal(env, v))
      case Parser.Seq(es) => Seq(es.map(evalNK(env, _)))
      case Parser.Sum(es) => Sum(es.map(evalNK(env, _)))
      case Parser.Intersection(e1, e2) => Intersection(evalNK(env, e1), evalNK(env, e2))
      case Parser.Difference(e1, e2) => Difference(evalNK(env, e1), evalNK(env, e2))
      case Parser.XOR(e1, e2) => XOR(evalNK(env, e1), evalNK(env, e2))
      case Parser.Star(e) => Star(evalNK(env, e))
      case Parser.Forward(e, negate) => val sp = Bisim.forward(evalNK(env, e)); TestSP(if negate then SP.negate(sp) else sp)
      case Parser.Backward(e, negate) => val sp = Bisim.backward(evalNK(env, e)); TestSP(if negate then SP.negate(sp) else sp)
      case Parser.Exists(x, e) => TestSP(SP.exists(x, toSP(evalNK(env, e))))
      case Parser.Forall(x, e) => TestSP(SP.forall(x, toSP(evalNK(env, e))))
      case Parser.VarName(x) =>
        if !env.contains(x) then throw new Throwable(s"Variable $x not found in $env\n")
        env(x) match {
          case Left(v) => v
          case Right(v) => throw new Throwable(s"Expected a netkat expression, but got a value: $v\n")
        }
    }

  def toSP(e: NK): SP =
    e match
      case TestSP(sp) => sp
      case Test(x, v) => SP.test(x, v)
      case TestNE(x, v) => SP.testNE(x, v)
      case _ => throw new Throwable(s"Expected a test, but got $e\n")

  def evalVal(env: Env, v: Parser.SVal): Int =
    v match {
      case Left(v) => v
      case Right(x) =>
        if !env.contains(x) then throw new Throwable(s"Variable $x not found in $env\n")
        env(x) match {
          case Left(v) => throw new Throwable(s"Expected a value, but got a netkat expression: $v\n")
          case Right(v) => v
        }
    }

  def eval(env: Env, e: Parser.Expr): Either[NK, Int] =
    e match {
      case Parser.Expr.NKExpr(e) => Left(evalNK(env, e))
      case Parser.Expr.ValExpr(v) => Right(evalVal(env, v))
    }

  def runStmt(env: Env, stmt: Parser.Stmt, path: String, line: Int): Env =
    def assertNK(e: Parser.Expr): NK =
      e match {
        case Parser.Expr.NKExpr(e) => evalNK(env, e)
        case Parser.Expr.ValExpr(v) => throw new Throwable(s"Expected a netkat expression, but got a value: $v\n")
      }
    stmt match {
      case Stmt.Check(op, e1, e2) => {
        val v1 = assertNK(e1)
        val v2 = assertNK(e2)
        if Options.convertToKat then appendToKatIndex(s"$path:${line + 1} ")
        val result = Bisim.bisim(v1, v2)
        assert(op == "≡" || op == "≢")
        if result == (op == "≡") then {
          println(s"\u001b[32mCheck passed in $path:${line + 1}\u001b[0m")
        } else {
          throw new Throwable(s"\u001b[31m!!! Check failed in $path:${line + 1} !!!\u001b[0m" ++ s"\nOperands were $v1 and $v2\n")
        }
        env
      }
      case Stmt.Run(method, e) =>
        val v = assertNK(e)
        method match {
          case "forward" =>
            val result = Bisim.forward(v)
            println(s"Forward at $path:${line + 1}: ${SP.pretty(result)}")
          case "backward" =>
            val result = Bisim.backward(v)
            println(s"Backward at $path:${line + 1}: ${SP.pretty(result)}")
        }
        env
      case Stmt.Let(x, e) => env + (x -> eval(env, e))
      case Stmt.Import(path2) =>
        // Here path2 is relative to path
        val path3 = path.split("/").dropRight(1).mkString("/") + "/" + path2
        runFile(env, path3)
      case Stmt.Print(e) =>
        val v = eval(env, e)
        println(s"Print at $path:${line + 1}: $v")
        env
      case Stmt.For(x, i0, i1, stmt) =>
        var env2 = env
        for (i <- i0 to i1) {
          env2 = runStmt(env2 + (x -> Right(i)), stmt, path, line)
        }
        env2
    }

  // Runs a whole file
  // Checks if the whole input was parsed, and gives an error otherwise
  // Also returns an error if the file could not be read
  def runFile(env: Env, path: String): Env =
    var env2 = env
    try {
      val input = scala.io.Source.fromFile(path).mkString.split("\n")
      // Iterate over each line
      for (i <- input.indices) {
        val line = input(i)
        if line.startsWith("--") || line.trim.isEmpty then ()
        else
          // Parse the line
          Parser.parseStmt(line) match {
            case Left(stmt, n) =>
              // Check if everything was parsed
              if n == line.length then {
                // Run the statement
                // try {
                env2 = runStmt(env2, stmt, path, i)
                // } catch {
                // case e: Throwable =>
                // println(s"Error in $path:${i + 1}: ${e.getMessage}\n")
                // }
              } else {
                // First split the input at the point where we stopped parsing
                val (left, right) = line.splitAt(n)
                throw new Throwable(s"Could not parse $path:${i + 1}: $left[!!!]$right\n")
              }
            case Right(msg) => throw new Throwable(s"Could not parse line $path:${i + 1}: ${msg}\n")
          }
      }
    } catch {
      case e: java.io.FileNotFoundException =>
        throw new Throwable(s"File $path not found\n")
      // case e: Throwable =>
      // print(e)
    }
    env2

  import java.io.FileWriter

  def runTopLevel(path: String) =
    clearCaches()
    println("Running " + path)
    val startTime = System.nanoTime()
    runFile(Map(), path)
    val endTime = System.nanoTime()
    val duration = (endTime - startTime) / 1_000_000_000.0
    val filename = path.split("/").last
    val msg = f"Execution time of $filename: ${duration}%.2f s \n"
    println(msg)

    // Append msg to benchresults.txt
    var fw = new FileWriter("benchresults/benchresults.txt", true) // true to append
    try {
      fw.write(msg)
    } finally {
      fw.close()
    }

    // append to benchresults/comparison.csv
    // system,file,time
    // katch,foo/bar.nkpl,2.23423
    fw = new FileWriter("benchresults/comparison.csv", true) // true to append
    try {
      fw.write(s"katch,$filename,$duration\n")
    } finally {
      fw.close()
    }

  def runTopLevelFrenetic(path: String) =
    println("Running Frenetic " + path)
    val startTime = System.nanoTime()
    // do ./runfrenetic.sh ${Options.katIndex()}
    import sys.process._
    val cmd = s"./runfrenetic.sh ${Options.katIndex()}"
    val exitCode = cmd.!
    val endTime = System.nanoTime()
    val duration = (endTime - startTime) / 1_000_000_000.0
    val filename = path.split("/").last
    val msg = f"Execution time of Frenetic on $filename: ${duration}%.2f s \n"
    println(msg)

    // Append msg to benchresults.txt
    var fw = new FileWriter("benchresults/benchresults.txt", true) // true to append
    try {
      fw.write(msg)
    } finally {
      fw.close()
    }

    fw = new FileWriter("benchresults/comparison.csv", true) // true to append
    try {
      fw.write(s"frenetic,$filename,$duration\n")
    } finally {
      fw.close()
    }

  def appendToKatIndex(nkplFile: String) =
    val fw = new java.io.FileWriter(s"kat/index.txt", true) // true to append
    fw.write(nkplFile + "\n")
    fw.close()

}
