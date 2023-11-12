import nkpl.Parser.Expr
import nkpl._

// val input = """x = (@dst←3 ∧ @pt←0 ⋅ @pt←0 ∪ @dst←4)? ⋆""" // your input string here
// // val input = "x = (@dst←3)"
// val result = Parser.parseStmt(input)
// val n = input.length

// val r = Parser.parseStmt("check (δ∪@e←5)⋅(δ⋅@e=5?) ≡ (@e←5∪δ)⋅(@e=5?⋅δ)")
// val a = r match { case Left((value, _)) => value }
// val (xx, yy) = a match { case Parser.Stmt.Check(op, Parser.Expr.NKExpr(e1), Parser.Expr.NKExpr(e2)) => (e1, e2) }
// val x = Runner.evalNK(Map(), xx)
// val y = Runner.evalNK(Map(), yy)

// Bisim.bisim(x, y)

// Bisim.ε(x)
// Bisim.ε(y)

// Bisim.δ(x)
// // Map((@e=5∪(δ⋅@e=5)) -> TestMut(e,Map(5 -> Map(5 -> Diag)),Map(),False), (δ⋅@e=5) -> TestMut(e,Map(5 -> Map()),Map(),Diag), @e=5 -> TestMut(e,Map(5 -> Map()),Map(5 -> Diag),False))
// Bisim.δ(y)
// // Map((ε∪(@e=5⋅δ)) -> TestMut(e,Map(5 -> Map(5 -> Diag)),Map(),False), (@e=5⋅δ) -> TestMut(e,Map(5 -> Map()),Map(),Diag), ε -> TestMut(e,Map(5 -> Map()),Map(5 -> Diag),False))

// val m = scala.collection.mutable.Map.empty[String, Int]
// m("hi") = 4
// println(m)
// m.clear()
// println(m)

// check (∅⋆∪@d←2⋆)⋅(@a=4??⋅@a←4)≡(∅⋆∪(ε∪@d←2⋅@d←2⋆))⋅@a=4??

def parse(s: String) =
  Parser.parseExpr(s) match {
    case Parser.Expr.NKExpr(nk) => Runner.evalNK(Map(), nk)
  }

val a = parse("ε")
Bisim.revTrans(a)

import scala.collection.immutable.HashMap

val x = Map(1 -> 2)

parse("rangesum @x 2 4")
fastparse.parse("33434 23", Parser.integer(_))
