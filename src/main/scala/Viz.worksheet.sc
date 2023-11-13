import nkpl.Parser.mut
import nkpl._

def parse(s: String) =
  Parser.parseExpr(s) match {
    case Parser.Expr.NKExpr(nk) => Runner.evalNK(Map(), nk)
  }

def canonSP(sp: SP): String =
  sp match {
    case SP.True => "⊤"
    case SP.False => "⊥"
    case SP.Test(x, ys, default) =>
      // print as (x=v0 ⋅ a_0 + x=v1 ⋅ a_1 + ... + x=vn ⋅ a_n + x≠v0 ⋅ ... x≠vn ⋅ a_{n+1})
      val z = VarMap(x)
      val ysStr = ys.map { case (v, a) => s"$z=$v⋅${canonSP(a)}" }.mkString(" + ")
      val defaultStr = ys.keys.map { v => s"$z≠$v" }.mkString("⋅") + "⋅" + canonSP(default)
      s"($ysStr + $defaultStr)"
  }

def canonTex(sp: SP): String =
  sp match {
    case SP.True => "\\top"
    case SP.False => "\\bot"
    case SP.Test(x, ys, default) =>
      // print as (x=v0 ⋅ a_0 + x=v1 ⋅ a_1 + ... + x=vn ⋅ a_n + x≠v0 ⋅ ... x≠vn ⋅ a_{n+1})
      val z = VarMap(x)
      val ysStr = ys.map { case (v, a) => s"$z=$v \\cdot ${canonTex(a)}" }.mkString(" + ")
      val defaultStr = ys.keys.map { v => s"$z \\neq $v" }.mkString("\\cdot") + "\\cdot" + canonTex(default)
      s"($ysStr + $defaultStr)"
  }

object GV {
  // graphviz construction helper object

  def genColor(y: String): String = {
    if y == "*" then return "#000000"
    val x = y + "arsotienaorisetnarstadtt"
    val r = 0
    val g = (x.hashCode % 256).abs
    val b = (x.hashCode / 256 % 256).abs
    // Convert RGB to hex #RRGGBB
    val colors = List("darkslateblue", "darkolivegreen", "darkgoldenrod", "darkcyan", "darkorchid", "darkred", "darkmagenta", "darkgreen", "midnightblue")
    // check if y is a number
    // if y.forall(_.isDigit) then colors(y.toInt % colors.length)
    // else f"#${r}%02x${g}%02x${b}%02x"
    colors(y.toInt)
    "black"
  }
  def genColor2(y: String) = {
    val x = y + "arsotienaorisetnarstadtt"
    val r = (x.hashCode % 256).abs
    val g = 200
    val b = (x.hashCode / 256 / 256 % 256).abs
    // Convert RGB to hex #RRGGBB
    if y == "a" then "skyblue"
    else if y == "b" then "#C1E1C1"
    else if y == "c" then "peachpuff"
    else f"#${r}%02x${g}%02x${b}%02x"
  }

  var syms = scala.collection.mutable.Map[Any, String]()
  def gensym(obj: Any) =
    syms.getOrElseUpdate(obj, s"n${syms.size}")

  def reset() =
    val n = syms.size
    syms.clear()
    for i <- 0 until n do syms(i) = s"dummy$i"

  val sb = new StringBuilder
  def line(text: String) =
    sb.append("   " + text + "\n")

  val edgeColor = "#555"
  val arrowhead = "normal"

  def edge(a: Any, b: Any, label: String = "") =
    line(s"""${gensym(a)} -> ${gensym(b)} [arrowhead=$arrowhead, label=" $label ", labelangle=-30, fontsize=12, arrowsize=0.5, color="$edgeColor", fontcolor="${genColor(label)}"]""")

  def defaultEdge(a: Any, b: Any, label: String = "") =
    // dotted edge
    line(s"""${gensym(a)} -> ${gensym(b)} [arrowhead=$arrowhead, arrowsize=0.5, style=dashed, color="$edgeColor"]""")

  def node(a: Any, label: String = "") =
    line(s"""${gensym(a)} [label="$label"]""")

  def decNode(a: Any, label: String = "") =
    if (label == "⊤") || (label == "⊥") then line(s"""${gensym(a)} [label="$label", shape=box, width=0.3, height=0.3, fixedsize=true]""")
    else line(s"""${gensym(a)} [label="$label", shape=circle, width=0.3, fixedsize=true, style=filled, fillcolor="${genColor2(label)}"]""")

  def nodeSP(a: Any, label: String) =
    decNode(a, label)

  def nodeSPP(a: Any, label: String) =
    decNode(a, label)

  def nodeSPPmuts(a: Any, label: String) =
    line(s"""${gensym(a)} [label="", shape=diamond, width=0.15, height=0.15, style=filled, fillcolor="${genColor2(label)}"]""")

  def sameRank(xs: List[Any]) =
    line(s"""{rank=same; ${xs.map(gensym).mkString("; ")}}""")

  def output() =
    val text = s"""
digraph G {
${sb.toString()}
}"""
    sb.clear()
    text

  def save(file: String) =
    val text = output()
    val pw = new java.io.PrintWriter(file)
    pw.write(text)
    pw.close()

  def show() =
    save("viz.gv")
    import sys.process._
    s"dot -Tpdf viz.gv -o viz.pdf".!
}

def gvSP(sp: SP) =
  lazy val gv: SP => Unit = memoize { sp =>
    sp match
      case SP.True => GV.nodeSP(sp, "⊤")
      case SP.False => GV.nodeSP(sp, "⊥")
      case SP.Test(x, ys, default) =>
        val z = VarMap(x)
        GV.nodeSP(sp, s"$z")
        ys.foreach { case (v, a) =>
          GV.edge(sp, a, s"$v")
          gv(a)
        }
        GV.defaultEdge(sp, default, s"≠")
        gv(default)
  }
  gv(sp)

// Now, we do SPPs
def canonSPP(spp: SPP): String =
  spp match {
    case SPP.Diag => "⊤"
    case SPP.False => "⊥"
    case SPP.TestMut(x, branches, muts, default) =>
      // Each branch contains muts
      val z = VarMap(x)
      val branchesStr = branches
        .map { case (v, muts) =>
          val mutsStr = muts.map { case (v2, spp) => s"$z←$v2 ⋅ ${canonSPP(spp)}" }.mkString(" + ")
          if mutsStr.isEmpty then s"$z=$v ⋅⊥"
          else s"$z=$v ⋅ ($mutsStr)"
        }
        .mkString(" + ")
      val mutsStr = muts.map { case (v, spp) => s"$z←$v ⋅ ${canonSPP(spp)}" }.mkString(" + ")
      val mutNeqs = muts.keys.map { v => s"$z≠$v" }.mkString("⋅")
      val defaultStr = if mutNeqs.isEmpty then canonSPP(default) else s"$mutNeqs ⋅ ${canonSPP(default)}"
      val neqs = branches.keys.map { v => s"$z≠$v" }.mkString("⋅")
      if branchesStr.isEmpty then s"($mutsStr + $defaultStr)"
      else if mutsStr.isEmpty then s"($branchesStr + $neqs ⋅ $defaultStr)"
      else s"($branchesStr + $neqs ⋅ ($mutsStr + $defaultStr))"
  }

def gvSPP(spp: SPP) =
  lazy val gv: SPP => Unit = memoize { spp =>
    spp match
      case SPP.Diag => GV.nodeSPP(spp, "⊤")
      case SPP.False => GV.nodeSPP(spp, "⊥")
      case SPP.TestMut(x, branches, muts, default) =>
        val z = VarMap(x)
        GV.nodeSPP(spp, s"$z")
        branches.foreach { case (v, muts) =>
          GV.nodeSPPmuts((spp, v), s"$z")
          GV.edge(spp, (spp, v), s"$v")
          muts.foreach { case (v2, spp2) =>
            GV.edge((spp, v), spp2, s"$v2")
            gv(spp2)
          }
        }
        GV.nodeSPPmuts((spp, "default"), s"$z")
        GV.defaultEdge(spp, (spp, "default"), s"≠")
        muts.foreach { case (v2, spp2) =>
          GV.edge((spp, "default"), spp2, s"$v2")
          gv(spp2)
        }
        GV.defaultEdge((spp, "default"), default, s"≠")
        // Make all muts the same rank
        GV.sameRank((branches.keys.map((spp, _)).toList ++ List((spp, "default"))).toList)
        gv(default)
  }
  gv(spp)

VarMap("a")
VarMap("b")
VarMap("c")

def test(x: String, y: Val) = SP.test(VarMap(x), y)
def testNE(x: String, y: Val) = SP.testNE(VarMap(x), y)

val x = SP.union(
  SP.intersection(test("a", 3), test("b", 4)),
  SP.intersection(test("c", 5), testNE("b", 5))
)

// another SP for example purposes, with different structure
val y = SP.union(
  SP.intersection(test("b", 3), test("c", 4)),
  SP.intersection(test("a", 5), testNE("c", 5))
)

canonSP(x)
canonSP(y)
// canonTex(x)

gvSP(x)
GV.reset()
gvSP(y)
GV.reset()
gvSP(SP.union(x, y))
GV.reset()

def testS(x: String, y: Val) = SPP.test(VarMap(x), y)
def testNES(x: String, y: Val) = SPP.testNE(VarMap(x), y)
def mutS(x: String, y: Val) = SPP.mut(VarMap(x), y)

def testSs(x: String, ys: List[Val]) = ys.foldRight(SPP.False: SPP) { case (y, sp) => SPP.union(testS(x, y), sp) }
def mutSs(x: String, ys: List[Val]) = ys.foldRight(SPP.False: SPP) { case (y, sp) => SPP.union(mutS(x, y), sp) }

val sx = SPP.union(
  SPP.union(testS("a", 3), SPP.intersection(testS("b", 4), mutSs("c", List(5, 6)))),
  SPP.intersection(
    SPP.union(testS("c", 5), testNES("b", 5)),
    SPP.union(mutS("a", 5), testS("c", 5))
  )
)

val sy = SPP.union(
  SPP.intersection(testSs("b", List(2, 4)), testS("c", 4)),
  SPP.intersection(testSs("a", List(1, 5)), mutSs("b", List(3, 5)))
)

canonSPP(sx)
gvSPP(sx)
GV.reset()

canonSPP(sy)
gvSPP(sy)
GV.reset()

canonSPP(SPP.union(sx, sy))
gvSPP(SPP.union(sx, sy))
GV.reset()

canonSPP(SPP.seq(sx, sy))
gvSPP(SPP.seq(sx, sy))
GV.reset()

canonSPP(SPP.difference(sx, sy))
gvSPP(SPP.difference(sx, sy))
GV.reset()

val sz = SPP.union(
  SPP.seq(testSs("a", List(1, 2)), mutSs("b", List(3, 4))),
  SPP.seq(testSs("b", List(3, 4)), mutSs("a", List(3)))
)

SPP.star(sz)

gvSPP(sz)
GV.reset()

gvSPP(SPP.star(sz))
GV.reset()

GV.show()
