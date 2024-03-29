package nkpl
import java.nio.file.{Files, Path, Paths}
import nkpl.Parser.Stmt
import nkpl.Parser.Expr

/** Print the first 100 characters of any object to be printed to the screen,
 *  for brevity.
 */
def summarize(obj: Any): String =
  val s = obj.toString
  if s.length > 100 then s.take(100) + "..."
  else s

/** Runner provides the functions and the environment needed for evaluating an NKPL program.
 */
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

  /** Convert a test-only NetKAT expression into an SP. */
  def toSP(e: NK): SP =
    e match
      case TestSP(sp) => sp
      case Test(x, v) => SP.test(x, v)
      case TestNE(x, v) => SP.testNE(x, v)
      case _ => throw new Throwable(s"Expected a test, but got $e\n")

  /** Lookup a variable in the environment. */
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

  /** Evaluate (recursively) a parse tree of an NKPL program. */
  def eval(env: Env, e: Parser.Expr): Either[NK, Int] =
    e match {
      case Parser.Expr.NKExpr(e) => Left(evalNK(env, e))
      case Parser.Expr.ValExpr(v) => Right(evalVal(env, v))
    }

  /** Evaluate a statement in NKPL. */
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
        val result = Bisim.bisim(v1, v2)
        assert(op == "≡" || op == "≢")
        if result == (op == "≡") then {
          if (!Options.suppressOutput) println(s"\u001b[32mCheck passed in $path:${line + 1}\u001b[0m")
        } else {
          throw new Throwable(s"\u001b[31m!!! Check failed in $path:${line + 1} !!!\u001b[0m" ++ s"\nOperands were ${summarize(v1.toString)} and ${summarize(v2.toString)}\n")
        }
        env
      }
      case Stmt.Graphviz(path2, e) => {
        val v = assertNK(e)
        val path3 = path.split("/").dropRight(1).mkString("/") + "/" + path2
        GV.saveNK(path3, v)
        if (!Options.suppressOutput) println(s"Graphviz at $path:${line + 1} saved in $path3: ${summarize(v)}")
        env
      }
      case Stmt.Run(method, e) =>
        val v = assertNK(e)
        method match {
          case "forward" =>
            val result = Bisim.forward(v)
            if (!Options.suppressOutput) println(s"Forward at $path:${line + 1}: ${summarize(SP.pretty(result))}")
          case "backward" =>
            val result = Bisim.backward(v)
            if (!Options.suppressOutput) println(s"Backward at $path:${line + 1}: ${summarize(SP.pretty(result))}")
        }
        env
      case Stmt.Let(x, e) => env + (x -> eval(env, e))
      case Stmt.Import(path2) =>
        // Here path2 is relative to path
        val path3 = path.split("/").dropRight(1).mkString("/") + "/" + path2
        runFile(env, path3)
      case Stmt.Print(e) =>
        val v = eval(env, e)
        println(s"Print at $path:${line + 1}: ${summarize(v)}")
        env
      case Stmt.For(x, i0, i1, stmt) =>
        var env2 = env
        for (i <- i0 to i1) {
          env2 = runStmt(env2 + (x -> Right(i)), stmt, path, line)
        }
        env2
    }

  /* Parse and evaluate a file specified by path. 
   * @param path
   * The path of the file.
   * @throws Throwable
   * in the event of parsing errors.
   */
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

  /** Run and measure the running time for an NKPL file. */
  def runTopLevel(path: String) =
    clearCaches()
    println("Running " + path)
    if Options.convertToKat then Files.deleteIfExists(Paths.get(Options.katIndex()))
    if Options.warmup then for (i <- 0 to 10) { runFile(Map(), path); clearCaches() }
    val reps = if Options.warmup then 100 else 0
    var time = 0.0
    for (i <- 0 to reps) {
      val startTime = System.nanoTime()
      runFile(Map(), path)
      val endTime = System.nanoTime()
      time += (endTime - startTime) / 1_000_000_000.0
      clearCaches()
    }
    val duration = time / (if Options.warmup then (0 to reps).length else 1)
    val filename = path.split("/").last
    val msg = f"Execution time of $filename: ${duration}%.2f s \n"
    println(msg)

    // Append msg to results.txt
    var fw = new FileWriter("results/results.txt", true) // true to append
    try {
      fw.write(msg)
    } finally {
      fw.close()
    }

    // Provided we are not in convert-to-kat mode, then
    // append to results/comparison.csv
    // system,file,time
    // katch,foo/bar.nkpl,2.23423
    if !Options.convertToKat then
      fw = new FileWriter(Options.outputCSV, true) // true to append
      try {
        fw.write(s"katch,$path,$duration\n")
      } finally {
        fw.close()
      }

  /** Run and measure, for comparison purposes, a query using `frenetic`. The
   *  implementation assumes we have already computed a query in frenetic's
   *  format.
   */
  def runTopLevelFrenetic(path: String) =
    println("Running Frenetic " + path)
    Options.inputFile = path
    val startTime = System.nanoTime()
    // do ./runfrenetic.sh ${Options.katIndex()}
    import sys.process._
    val cmd = s"./scripts/runfrenetic.sh ${Options.katIndex()} ${Options.freneticTimeout}"
    val exitCode = cmd.!
    val endTime = System.nanoTime()
    var timedOut = false
    if exitCode == 137 then timedOut = true
    else if exitCode != 0 then
      println("runfrenetic failed!")
      System.exit(exitCode)
    val duration = (endTime - startTime) / 1_000_000_000.0
    val filename = path.split("/").last

    var durationMsg = ""
    if (timedOut) then durationMsg = s"timeout (${Options.freneticTimeout})"
    else durationMsg = f"${duration}%.2f"
    val msg = f"Execution time of Frenetic on $filename: $durationMsg s\n"
    println(msg)

    // Append msg to results.txt
    var fw = new FileWriter("results/results.txt", true) // true to append
    try {
      fw.write(msg)
    } finally {
      fw.close()
    }

    fw = new FileWriter(Options.outputCSV, true) // true to append
    try {
      fw.write(s"frenetic,$path,$durationMsg\n")
    } finally {
      fw.close()
    }

}
