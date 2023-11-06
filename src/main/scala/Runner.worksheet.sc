import nkpl._

def ignore(x: Any): Unit = ()

// Runner.runTopLevel("examples/topologyzoo/Arpa_topo.nkpl")
// ignore(Runner.runTopLevel("benchmarks/large/Deltacom_reachability.nkpl"))
// Runner.runTopLevel("benchmarks/tiny/t4_h_reachability.nkpl")
// Runner.runTopLevel("benchmarks/large/ft6_slicing.nkpl")
// Runner.runTopLevel("benchmarks/tiny/t4_slicing.nkpl")

// ignore(Runner.runTopLevel("nkpl/test.nkpl"))

import SPP._

val a = "a"
Bisim.δ(Seq(List(Test(a, 1), Mut(a, 2), Dup, Test(a, 2))))

Bisim.ε(Seq(List(Test(a, 2), Dup)))
Bisim.δ(Seq(List(Test(a, 2), Dup)))
Bisim.ε(Seq(List()))
Bisim.δ(Seq(List()))

Bisim.ε(Sum(Set()))
Bisim.δ(Sum(Set()))

Bisim.bisim(Seq(List(Test(a, 2), Dup)), Sum(Set()))

SP.difference(SP.True, SP.Test(a, Map(2 -> SP.True), SP.False))
SP.difference(SP.Test(a, Map(2 -> SP.True), SP.False), SP.False)

// check @a=1 ⋅ ((@a=1 ⋅ @a←2 ⋅ δ) ∪ (@a=2 ⋅ @a←3 ⋅ δ))⋆ ⋅ @a=3 ≢  ∅

val e12 = Seq(List(Test(a, 1), Mut(a, 2), Dup))
val e23 = Seq(List(Test(a, 2), Mut(a, 3), Dup))

Bisim.bisim(
  Seq(List(Test(a, 1), Star(Sum(Set(e12, e23))), Test(a, 3))),
  Sum(Set())
)

Bisim.ε(Seq(List(Test(a, 1), Star(Sum(Set(e12, e23))), Test(a, 3))))
Bisim.δ(Seq(List(Test(a, 1), Star(Sum(Set(e12, e23))), Test(a, 3))))

def parseExpr(s: String): NK =
  val env = Runner.runStmt(Map(), Parser.Stmt.Let("result", Parser.parseExpr(s)), "test", 0)
  env("result") match {
    case Left(e) => e
    case Right(v) => throw new Throwable(s"Expected a netkat expression, but got a value: $v\n")
  }
val zz = parseExpr("(((@a=1⋅@a←2⋅δ)∪(@a=2⋅@a←3⋅δ))⋆⋅@a=3)")
Bisim.ε(zz)
Bisim.δ(zz)

SPP.run(
  SP.Test(a, Map(2 -> SP.True), SP.False),
  TestMut(a, Map(2 -> Map(3 -> Diag)), Map(), False)
)

// val lhs = parseExpr("(δ∪@e←5)⋅(δ⋅@e=5?)")
// val rhs = parseExpr("(@e←5∪δ)⋅(@e=5?⋅δ)")

val lhs = parseExpr("(δ∪@e←5)⋅δ")
val rhs = parseExpr("(@e←5∪δ)⋅δ")

Bisim.bisim(lhs, rhs)
Bisim.bisim(rhs, lhs)
Bisim.bisim(rhs, rhs)
Bisim.bisim(lhs, lhs)
Bisim.ε(lhs)
Bisim.ε(rhs)
Bisim.δ(lhs)
Bisim.δ(rhs)
SMap.canonicalize(Bisim.δ(rhs))

val aa = TestMut("e", Map(), Map(5 -> Diag), False)
SPP.difference(Diag, aa)
SPP.difference(aa, Diag)
SPP.intersection(aa, Diag)
SPP.intersection(Diag, aa)

SPP.run(SP.True, SPP.difference(Diag, aa))
SP.difference(SPP.run(SP.True, Diag), SPP.run(SP.True, aa))

// Map(
// Seq(List(Star(Sum(Set(Seq(List(Test(a,1), Mut(a,2), Dup)), Seq(List(Test(a,2), Mut(a,3), Dup))))), Test(a,3))) ->
// TestMut(a,Map(1 -> Map(2 -> Diag)),Map(),False)
// )

// Map(Sum(Set()) -> Diag, Test(a,2) -> TestMut(a,Map(1 -> Map(2 -> Diag)),Map(),False))
// Map(Sum(Set(Seq(List(Sum(Set()), Dup, Test(a,2))), Seq(List(Sum(Set()), Mut(a,2), Dup, Test(a,2))))) -> TestMut(a,Map(1 -> Map(1 -> Diag)),Map(),False), Seq(List(Sum(Set()), Mut(a,2), Dup, Test(a,2))) -> TestMut(a,Map(1 -> Map()),Map(),Diag), Test(a,2) -> TestMut(a,Map(1 -> Map(2 -> Diag)),Map(),False))

// val sL = TestMut(a, Map(), Map(3 -> Diag, 4 -> Diag), False)
// val sR = TestMut(a, Map(4 -> Map(4 -> Diag)), Map(3 -> Diag, 4 -> Diag), False)
// seq(sL, sR)
// seq(sR, sL)

// val s3 = TestMut(a, Map(4 -> Map(3 -> Diag)), Map(3 -> Diag, 4 -> Diag), False)
// union(Diag, s3)

// val x = TestMut(a, Map(3 -> Map(3 -> Diag, 4 -> Diag)), Map(3 -> Diag), False)

// union(x, Diag)

// val y = TestMut(a, Map(3 -> Map(3 -> Diag), 4 -> Map(4 -> Diag)), Map(3 -> Diag, 4 -> Diag), Diag)
// seq(y, x)

// star(x)

// val r = union(mut(a, 3), seq(test(a, 3), mut(a, 4)))
// star(r)

// val l = union(union(mut(a, 3), mut(a, 4)), Diag)
// l
// val q = union(mut(a, 3), mut(a, 4))
// union(Diag, q)
// union(Diag, q) eq star(r)

// seq(Diag, False)
// seq(False, Diag)
// run(SP.True, TestMut(a, Map(), Map(3 -> Diag, 4 -> Diag), Diag))

// Bisim.ε(Sum(Set(Mut(a, 3), Mut(a, 4), Seq(List()))))
// Bisim.ε(Star(Sum(Set(Mut(a, 3), Seq(List(Test(a, 3), Mut(a, 4)))))))
// Bisim.δ(Sum(Set(Mut(a, 3), Mut(a, 4), Seq(List()))))
// Bisim.δ(Seq(List()))
// Bisim.δ(Mut(a, 3))
