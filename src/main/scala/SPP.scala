package nkpl
import scala.collection.immutable.HashMap

class SP

object SP {
  case object True extends SP
  case object False extends SP
  case class Test(x: Var, ys: HashMap[Val, SP], default: SP) extends SP {
    // Cache the hashcode
    override val hashCode = x.hashCode + ys.hashCode + default.hashCode
  }

  object Test {
    val cache = scala.collection.mutable.HashMap.empty[Test, Test]
    def apply(x: Var, ys: HashMap[Val, SP], default: SP) =
      val ys2 = ys.filterNot { (v, y) => y eq default }
      if ys2.isEmpty then default
      else
        val sp = new Test(x, ys2, default)
        cache.getOrElseUpdate(sp, sp)

    def mk(x: Var, ys: HashMap[Val, SP], default: SP) =
      if ys.isEmpty then default
      else
        val sp = new Test(x, ys, default)
        cache.getOrElseUpdate(sp, sp)
  }

  def pretty(sp: SP): String =
    var n = 0
    val sb = new StringBuilder
    lazy val pp: SP => String = memoize { sp =>
      sp match
        case False => "False"
        case True => "True"
        case Test(x, ys, default) =>
          val ysi = ys.map((v, sp) => s"$v -> ${pp(sp)}").mkString(", ")
          val defaulti = pp(default)
          n += 1
          sb.append(s"var x$n = Test($x, Map($ysi), $defaulti); ")
          s"x$n"
    }
    sb.append(pp(sp))
    sb.toString

  lazy val union: (SP, SP) => SP = memoize2 { (x, y) => unionPrim(x, y) }
  def unionPrim(x: SP, y: SP): SP =
    if x eq y then return x
    (x, y) match {
      case (False, _) => y
      case (True, _) => True
      case (Test(xL, ysL, defaultL), Test(xR, ysR, defaultR)) =>
        if xL == xR then
          val keys = ysL.keySet ++ ysR.keySet
          val ys = keys.map { v => v -> union(ysL.getOrElse(v, defaultL), ysR.getOrElse(v, defaultR)) }.to(HashMap)
          Test(xL, ys, union(defaultL, defaultR))
        else if xL < xR then Test(xL, ysL.map { (v, a) => v -> union(a, y) }, union(defaultL, y))
        else union(y, x)
      case _ => union(y, x)
    }

  def unionN(xs: Iterable[SP]): SP =
    xs.foldLeft(False: SP)(union(_, _))

  lazy val negate: SP => SP = memoize { x => negatePrim(x) }
  def negatePrim(x: SP): SP =
    x match {
      case False => True
      case True => False
      case Test(x, ys, default) => Test(x, ys.map { (v, a) => v -> negate(a) }, negate(default))
    }

  lazy val difference: (SP, SP) => SP = memoize2 { (x, y) => differencePrim(x, y) }
  def differencePrim(x: SP, y: SP): SP =
    (x, y) match {
      case (False, _) => False
      case (_, False) => x
      case (_, True) => False
      case (True, _) => negate(y)
      case (Test(xL, ysL, defaultL), Test(xR, ysR, defaultR)) =>
        if xL == xR then
          if (defaultL eq False) && (defaultR eq False) then
            var newbranches = HashMap.empty[Val, SP]
            for (v, sp) <- ysL do
              if ysR.contains(v) then
                val sp2 = difference(sp, ysR(v))
                if !(sp2 eq False) then newbranches = newbranches.updated(v, sp2)
              else newbranches = newbranches.updated(v, sp)
            return Test(xL, newbranches, False)
          val keys = ysL.keySet ++ ysR.keySet
          val ys = keys.map { v => v -> difference(ysL.getOrElse(v, defaultL), ysR.getOrElse(v, defaultR)) }.to(HashMap)
          Test(xL, ys, difference(defaultL, defaultR))
        else if xL < xR then Test(xL, ysL.map { (v, a) => v -> difference(a, y) }, difference(defaultL, y))
        else Test(xR, ysR.map { (v, a) => v -> difference(x, a) }, difference(x, defaultR))
    }

  lazy val intersection: (SP, SP) => SP = memoize2 { (x, y) => intersectionPrim(x, y) }
  def intersectionPrim(x: SP, y: SP): SP =
    (x, y) match {
      case (False, _) => False
      case (True, _) => y
      case (Test(xL, ysL, defaultL), Test(xR, ysR, defaultR)) =>
        if xL == xR then
          val keys = ysL.keySet ++ ysR.keySet
          val ys = keys.map { v => v -> intersection(ysL.getOrElse(v, defaultL), ysR.getOrElse(v, defaultR)) }.to(HashMap)
          Test(xL, ys, intersection(defaultL, defaultR))
        else if xL < xR then Test(xL, ysL.map { (v, a) => v -> intersection(a, y) }, intersection(defaultL, y))
        else intersection(y, x)
      case _ => intersection(y, x)
    }

  def xor(x: SP, y: SP): SP =
    union(difference(x, y), difference(y, x)) // FIXME: optimize this

  def intersectionN(xs: Iterable[SP]): SP =
    xs.foldLeft(True: SP)(intersection(_, _))

  def elemOf(packet: Map[Var, Val], x: SP): Boolean =
    x match {
      case SP.True => true
      case SP.False => false
      case SP.Test(v, ys, default) =>
        if packet.contains(v) then
          ys.get(packet(v)) match {
            case Some(a) => elemOf(packet, a)
            case None => elemOf(packet, default)
          }
        else elemOf(packet, default)
    }

  lazy val exists: (Var, SP) => SP = memoize2 { (x, sp) => existsPrim(x, sp) }
  def existsPrim(x: Var, sp: SP): SP =
    sp match {
      case SP.False => False
      case SP.True => True
      case SP.Test(y, ys, default) =>
        if x == y then union(unionN(ys.values), default)
        else Test(y, ys.map { (v, sp) => v -> exists(x, sp) }, exists(x, default))
    }

  lazy val forall: (Var, SP) => SP = memoize2 { (x, sp) => forallPrim(x, sp) }
  def forallPrim(x: Var, sp: SP): SP =
    sp match {
      case SP.False => True
      case SP.True => True
      case SP.Test(y, ys, default) =>
        if x == y then intersection(intersectionN(ys.values), default)
        else Test(y, ys.map { (v, sp) => v -> forall(x, sp) }, forall(x, default))
    }

  def test(x: Var, y: Val): SP = Test(x, HashMap(y -> True), False)
  def testNE(x: Var, y: Val): SP = Test(x, HashMap(y -> False), True)

  def logSumSP(msg: String, sp1: SP, sp2: SP): Unit =
    def spStr(sp: SP) =
      sp match
        case SP.False => "F"
        case SP.True => "T"
        case SP.Test(x, ys, default) =>
          val ls = ys.size
          val lm = if default eq SP.False then "F" else if default eq SP.True then "T" else "?"
          s"($x,$ls,$lm)"
    println(s"$msg ${spStr(sp1)} ${spStr(sp2)}")
}

class SPP

object SPP {
  def logSPP(x: String): Unit =
    // println(x)
    ()

  case object Diag extends SPP
  case object False extends SPP
  case class TestMut(x: Var, branches: HashMap[Val, HashMap[Val, SPP]], other: HashMap[Val, SPP], id: SPP) extends SPP {
    // Cache the hashcode
    override val hashCode = x.hashCode + branches.hashCode + other.hashCode + id.hashCode
  }
  object TestMut {
    // FIXME: try the Java hashmap here, and use identity hash
    val cache = scala.collection.mutable.HashMap.empty[TestMut, TestMut]
    // val cache = scala.collection.mutable.HashMap.empty[TestMut, TestMut]
    def apply(x: Var, branches: HashMap[Val, HashMap[Val, SPP]], other: HashMap[Val, SPP], id: SPP): SPP =
      // Remove redundant branches
      // A branch is redundant if sending the value to other/id would do the same thing

      // First, we remove muts that are False from branches
      var branches2 = branches.map { (v, muts) => v -> muts.filterNot { (v2, spp) => spp eq False } }

      // Now we remove muts that are False from other
      // We have to be careful here, because removing a False mut from other may
      // cause a packet to go to id instead of other.
      for (v, spp) <- other do
        if (spp eq False) && !branches2.contains(v)
        then branches2 = branches2.updated(v, HashMap.empty)
      val other2 = other.filterNot { (v, spp) => spp eq False }

      // Now we can check for and remove branches that are the semantically the same as the
      // default case. Removing this branch means that packets that would follow
      // this branch will go through the other branch. From here, the other
      // branch either peforms a mutation or not. In other words, both the
      // updates that change the packet value need to have the same effect as
      // default, and those that do not change the packet value.
      val branches3 = branches2.filter { (v, muts) =>
        muts != (if other2.contains(v) || (id eq False) then other2 else other2 + (v -> id))
      }
      if branches3.isEmpty && other2.isEmpty then return id
      val v = new TestMut(x, branches3, other2, id)
      cache.getOrElseUpdate(v, v)
    def mk(x: Var, branches: HashMap[Val, HashMap[Val, SPP]], other: HashMap[Val, SPP], id: SPP): SPP =
      val v = new TestMut(x, branches, other, id)
      cache.getOrElseUpdate(v, v)
  }

  // Semantics:
  // Let the value of field x in the incoming packet be v.
  // If v is in branches.keySet, then we nondeterministically mutate x to one of the values in branches(x).keySet and continue with the SPP in branches(x)(v).
  // If the value of x is not in branches.keySet, then we nondeterministically
  // either mutate x to one of the values in other.keySet and continue with the SPP in other(v),
  // or we do not mutate x and continue with the SPP in id.

  // The function below implements the semantics of SPPs.
  // It is only used for testing / explanation purposes.

  def run1(p: Map[Var, Val], spp: SPP): Set[Map[Var, Val]] =
    // logSPP(s"run1($p, $spp)")
    spp match {
      case Diag => Set(p)
      case False => Set()
      case TestMut(x, branches, other, id) =>
        val v = p(x)
        if branches.contains(v) then
          val muts = branches(v)
          muts.keySet.flatMap { v2 => run1(p.updated(x, v2), muts(v2)) }
        else
          val others = other.keySet.flatMap { v2 => run1(p.updated(x, v2), other(v2)) }
          if other.contains(v) then others else others ++ run1(p, id) // do we want this? I think so
      // else
      //   other.keySet.flatMap { v2 =>
      //     if v == v2 then Set() else run1(p.updated(x, v2), other(v2))
      //   } ++ run1(p, id)
    }

  def test(x: Var, y: Val): SPP =
    TestMut(x, HashMap(y -> HashMap(y -> Diag)), HashMap.empty, False)

  def testNE(x: Var, y: Val): SPP =
    TestMut(x, HashMap(y -> HashMap.empty), HashMap.empty, Diag)

  def mut(x: Var, y: Val): SPP =
    TestMut(x, HashMap.empty, HashMap(y -> Diag), False)
    // TestMut(x, Map(y -> Map(y -> Diag)), Map(y -> Diag), False)

  def logSummarySP(msg: String, sp: SP, spp: SPP): Unit =
    val spstr = sp match {
      case SP.False => "F"
      case SP.True => "T"
      case SP.Test(x, ys, default) =>
        val ls = ys.size
        val lm = if default eq SP.False then "F" else if default eq SP.True then "T" else "?"
        s"($x,$ls,$lm)"
    }
    val sppstr = spp match {
      case Diag => "D"
      case False => "F"
      case TestMut(x, branches, other, id) =>
        val ls = branches.size
        val lls = branches.map { (_, a) => a.size }.sum
        val lm = other.size
        val idls = if id eq False then "F" else if id eq Diag then "D" else "?"
        s"($x,$ls/$lls,$lm,$idls)"
    }
    println(s"$msg $spstr, $sppstr")

  // Runs a symbolic packet through an SPP, and returns the symbolic packet that results.
  lazy val run: (SP, SPP) => SP = memoize2 { (sp, spp) => runPrim(sp, spp) }
  def runPrim(sp: SP, spp: SPP): SP =
    // logSPP(s"run($sp, $spp)")
    (sp, spp) match {
      case (SP.False, _) => SP.False
      case (_, False) => SP.False
      case (_, Diag) => sp
      case (SP.True, TestMut(x, branches, other, id)) =>
        // logSummarySP("run/True", sp, spp)
        // Since the packet can contain anything, we need to nondeterministically
        // go down all branches. It can also contain a value not in branches.keySet,
        // so we need to go down the other branch as well, and we need to go down id.
        // However, we only go down id if the output packet contains a value not in other.keySet.
        val branchesA = unionMapsSP(branches.map { (_, muts) =>
          muts.map { (v, spp) => v -> run(SP.True, spp) }
        })
        val branchesB = other.map { (v, spp) => v -> run(SP.True, spp) }
        var branchesC = unionMapSP(branchesA, branchesB)
        // We go to id only if the input packet is not matched by branches or other.
        for v <- (branches.keySet ++ other.keySet) -- branchesC.keySet do branchesC = branchesC.updated(v, SP.False)
        val spid = run(SP.True, id)
        for v <- branchesC.keySet -- (branches.keySet ++ other.keySet) do branchesC = branchesC.updated(v, SP.union(branchesC(v), spid))
        SP.Test(x, branchesC.to(HashMap), run(SP.True, id))
      case (SP.Test(x, ys, default), TestMut(x2, branches, other, id)) =>
        if x == x2 then
          if (default eq SP.False) && (id eq SPP.False) && other.isEmpty then
            var newbranches = HashMap.empty[Val, SP]
            for (v, sp) <- ys do
              if branches.contains(v) then
                val muts = branches(v)
                for (v2, spp) <- muts do
                  val sp2 = run(sp, spp)
                  if !(sp2 eq SP.False) then newbranches = newbranches.updated(v2, SP.union(sp2, newbranches.getOrElse(v2, SP.False)))
            return SP.Test.mk(x, newbranches, SP.False)
          // logSummarySP("run/=", sp, spp)
          // We must correlate the values in ys with the values in branches.
          // If a value is in both ys and branches, then we must go down the branch.
          // If a value is in ys but not in branches, then we must go down the other/id branch.
          // If a value is in branches but not in ys, then we must go down the branch with default.
          // If a value is in neither ys nor branches, then we must go down the branch with default.
          val branchesA = SP.unionN(ys.map { (v, sp) =>
            if branches.contains(v) then SP.Test(x, branches(v).map { (v2, spp) => v2 -> run(sp, spp) }, SP.False)
            else
              var zs = other.map { (v2, spp) => v2 -> run(sp, spp) }
              if !other.contains(v) then zs += v -> run(sp, id) // can optimize this
              SP.Test(x, zs, SP.False)
          })
          // Here we know that the input packet x field is not in ys.keySet.
          // So we are in the default case.
          // First, we look at the branches that are not in ys.keySet
          val branchesB = SP.unionN((branches -- ys.keySet).map { (_, muts) =>
            SP.Test(x, muts.map { (v, spp) => v -> run(default, spp) }, SP.False)
          })
          // Now we look at other/id
          val branchesC = SP.Test(x, ys.map { (v, _) => v -> SP.False } ++ branches.map { (v, _) => v -> SP.False } ++ other.map { (v, spp) => v -> run(default, spp) }, run(default, id))
          SP.union(branchesA, SP.union(branchesB, branchesC))
        else if x < x2 then
          // logSummarySP("run/<", sp, spp)
          // Here we need to nest the test for x2 inside the test for x:
          // SP.Test(x, ... SP.Test(x2, ...) ...)
          // This is like the equality case, but with empty branches and other, and id=spp
          SP.Test(x, ys.map { (v, sp2) => v -> run(sp2, spp) }, run(default, spp))
        else
          // logSummarySP("run/>", sp, spp)
          // Here we need to nest the test for x inside the test for x2:
          // SP.Test(x2, ... SP.Test(x, ...) ...)
          // But the input packet doesn't test x at all.
          // So this is like the equality case, but with empty ys.
          val branchesB = SP.unionN(branches.map { (_, muts) =>
            SP.Test(x2, muts.map { (v, spp) => v -> run(sp, spp) }, SP.False)
          })
          // Now we look at other/id
          val branchesC = SP.Test(x2, branches.map { (v, _) => v -> SP.False } ++ other.map { (v, spp) => v -> run(sp, spp) }, run(sp, id))
          SP.union(branchesB, branchesC)
    }

  def fromSP(sp: SP): SPP =
    sp match {
      case SP.False => False
      case SP.True => Diag
      case SP.Test(x, ys, default) =>
        TestMut(x, ys.map { (v, sp) => v -> HashMap(v -> fromSP(sp)) }, HashMap.empty, fromSP(default))
    }

  lazy val toSPforward: SPP => SP = memoize { spp => toSPforwardPrim(spp) }
  def toSPforwardPrim(spp: SPP): SP =
    spp match {
      case False => SP.False
      case Diag => SP.True
      case TestMut(x, branches, other, id) =>
        val branches2 = unionMapSP(
          unionMapsSP(branches.map { (_, muts) => muts.map { (v, spp) => v -> toSPforward(spp) } }),
          other.map { (v, spp) => v -> toSPforward(spp) }
        )
        val branches3 = unionMapSP(
          (branches2.keySet -- (branches.keySet ++ other.keySet)).map { v => v -> toSPforward(id) }.toMap,
          ((branches.keySet ++ other.keySet) -- branches2.keySet).map { v => v -> SP.False }.toMap
        )
        SP.Test(x, unionMapSP(branches2, branches3).to(HashMap), toSPforward(id))
    }

  // This is equivalent to SPP.run, but it is implemented differently (SPP.run is more efficient, but this is easier to understand).
  lazy val push: (SPP, SP) => SP = memoize2 { (spp, sp) => pushPrim(spp, sp) }
  def pushPrim(spp: SPP, sp: SP): SP =
    toSPforward(seq(fromSP(sp), spp))

  lazy val toSPbackward: SPP => SP = memoize { spp => toSPbackwardPrim(spp) }
  def toSPbackwardPrim(spp: SPP): SP =
    spp match {
      case False => SP.False
      case Diag => SP.True
      case TestMut(x, branches, other, id) =>
        val ys1 = branches.map { (v, muts) => v -> SP.unionN(muts.map((_, spp) => toSPbackward(spp))) }
        val y = SP.unionN(other.map((_, spp) => toSPbackward(spp)))
        val ys2 = (other -- branches.keySet).map { (v, _) => v -> y }
        val default = SP.union(y, toSPbackward(id))
        SP.Test(x, ys1 ++ ys2, default)
    }

  // Runs a symbolic packet through an SPP backward, and returns the symbolic input packet that results.
  // We want all input packets that can result in the given output packet being produced.
  lazy val pull: (SPP, SP) => SP = memoize2 { (spp, sp) => pullPrim(spp, sp) }
  def pullPrim(spp: SPP, sp: SP): SP =
    toSPbackward(seq(spp, fromSP(sp)))
    // (spp, sp) match
    //   case (_, SP.False) => SP.False
    //   case (False, _) => SP.False
    //   case (Diag, _) => sp
    //   case (TestMut(x, branches, muts, id), SP.True) => pull(spp, new SP.Test(x, HashMap.empty, SP.True))
    //   case (TestMut(x, branches, muts, id), SP.Test(y, branchesR, default)) =>
    //     if x == y then
    //       ???
    //       ???
    //     else if x < y then pull(spp, new SP.Test(y, HashMap.empty, sp))
    //     else pull(new TestMut(y, HashMap.empty, HashMap.empty, spp), sp)

  def unionMapsSP(xs: Iterable[Map[Val, SP]]): Map[Val, SP] =
    var m = scala.collection.mutable.Map[Val, SP]()
    for x <- xs; (v, sp) <- x do
      if m.contains(v) then m(v) = SP.union(m(v), sp)
      else m(v) = sp
    m.to(HashMap)

  def unionMapSP(xs: Map[Val, SP], ys: Map[Val, SP]): Map[Val, SP] =
    var m = scala.collection.mutable.Map.from(xs)
    for (v, sp) <- ys do
      if m.contains(v) then m(v) = SP.union(m(v), sp)
      else m(v) = sp
    m.to(HashMap)

  // Converts a symbolic packet into a test SPP
  def toTest(sp: SP): SPP =
    sp match
      case SP.False => SPP.False
      case SP.True => SPP.Diag
      case SP.Test(x, ys, default) =>
        SPP.TestMut(x, ys.map { (v, sp) => v -> HashMap(v -> toTest(sp)) }, HashMap.empty, toTest(default))

  // Composes a test represented as a SP with a SPP.
  // We could implement this by converting the SP to a SPP and then composing the SPPs.
  def seqSP(sp: SP, spp: SPP): SPP =
    // logSPP(s"seqSP($sp, $spp)")
    (sp, spp) match {
      case (SP.False, _) => False
      case (SP.True, _) => spp
      case (_, False) => False
      case (_, Diag) => toTest(sp)
      case (SP.Test(x, ys, default), TestMut(x2, branches, other, id)) =>
        if x == x2 then
          val branches2 = (ys.keySet ++ branches.keySet ++ other.keySet)
            .map { case v =>
              val y = ys.getOrElse(v, default)
              val b = branches.getOrElse(v, if other.contains(v) then other else other + (v -> id))
              v -> b.map { (v2, spp) => v2 -> seqSP(y, spp) }
            }
            .to(HashMap)
          val other2 = other.map { (v2, spp) => v2 -> seqSP(default, spp) }
          val id2 = seqSP(default, id)
          TestMut(x, branches2, other2, id2)
        else if x < x2 then
          seqSP(sp, new TestMut(x, HashMap.empty, HashMap.empty, spp))
          // val branches2 = ys.map { (v, sp) => v -> Map(v -> seqSP(sp, spp)) }
          // TestMut(x, branches2, HashMap.empty, seqSP(default, spp))
        else seqSP(new SP.Test(x2, HashMap.empty, sp), spp)
      // val branches2 = branches.map { (v, muts) =>
      //   v -> muts.map { (v2, spp) => v2 -> seqSP(sp, spp) }
      // }
      // val other2 = other.map { (v, spp) => v -> seqSP(sp, spp) }
      // TestMut(x2, branches2, other2, seqSP(sp, id))
    }

  def get(x: SPP, v: Val): HashMap[Val, SPP] =
    x match {
      case TestMut(x, branches, other, id) =>
        if branches.contains(v) then branches(v)
        else if other.contains(v) || (id eq False) then other
        else other + (v -> id)
      case False => HashMap.empty
      case Diag => HashMap(v -> Diag)
    }

  def logSummary(msg: String, spp1: SPP, spp2: SPP): Unit =
    (spp1, spp2) match {
      case (TestMut(xL, branchesL, mutsL, idL), TestMut(xR, branchesR, mutsR, idR)) =>
        val ls = branchesL.size
        val lls = branchesL.map { (_, a) => a.size }.sum
        val rs = branchesR.size
        val rrs = branchesR.map { (_, a) => a.size }.sum
        val lm = mutsL.size
        val rm = mutsR.size
        val idls = if idL eq False then "F" else if idL eq Diag then "D" else "?"
        val idrs = if idR eq False then "F" else if idR eq Diag then "D" else "?"
        val isin = if (branchesR.size == 1) && !branchesL.contains(branchesR.head._1) && mutsR.size == 0 && idL == False && idR == False then "Y" else ""
        println(s"$msg ($xL,$ls/$lls,$lm,$idls), ($xR,$rs/$rrs,$rm,$idrs) $isin")
      case _ => ()
    }

  // def union(x: SPP, y: SPP): SPP =
  lazy val union: (SPP, SPP) => SPP = memoize2 { (x, y) => unionPrim(x, y) }
  def unionPrim(x: SPP, y: SPP): SPP =
    // logSPP(s"union($x, $y)")
    if x eq y then return x
    (x, y) match {
      case (False, _) => y
      case (_, False) => x
      case (Diag, TestMut(x, branches, other, id)) =>
        // println("union Diag")
        union(new TestMut(x, HashMap.empty, HashMap.empty, Diag), y)
      // var branches2 = branches.map { (v, muts) =>
      //   // Add Diag to the diagonal
      //   v -> muts.updated(v, union(Diag, muts.getOrElse(v, False)))
      // }
      // // Also add Diag to all the values in other
      // // We need to turn them into tests for this
      // for (v, spp) <- other do
      //   if !branches2.contains(v)
      //   then branches2 = branches2.updated(v, other.updated(v, union(Diag, spp)))
      // val id2 = union(Diag, id)
      // TestMut(x, branches2, other, id2)
      case (TestMut(xL, branchesL, mutsL, idL), TestMut(xR, branchesR, mutsR, idR)) =>
        if xL == xR then
          if (idL eq False) && (idR eq False) then
            if branchesL.isEmpty && branchesR.isEmpty then
              if mutsR.size == 1 && !mutsL.contains(mutsR.head._1) then return TestMut.mk(xL, branchesL, mutsL.updated(mutsR.head._1, mutsR.head._2), idL)
              else return TestMut(xL, branchesL, unionMap(mutsL, mutsR), idL)
            if mutsL.isEmpty && branchesR.size == 1 && !branchesL.contains(branchesR.head._1) && mutsR.size == 0 then return TestMut.mk(xL, branchesL.updated(branchesR.head._1, branchesR.head._2), mutsL, idL)

          // else return TestMut.mk(xL, branchesL, mutsL.updated(mutsR.head._1, union(mutsR.head._2, mutsL(mutsR.head._1))), idL)
          // logSummary("union=", x, y)
          val branches = (branchesL.keySet ++ branchesR.keySet ++ mutsL.keySet ++ mutsR.keySet)
            .map { v =>
              v -> unionMap(get(x, v), get(y, v))
            }
            .to(HashMap)
          val muts = unionMap(mutsL, mutsR)
          val id = union(idL, idR)
          TestMut(xL, branches, muts, id)
        else if xL < xR then
          // logSummary("union<", x, y)
          union(x, new TestMut(xL, HashMap.empty, HashMap.empty, y))
          // var branches = branchesL.map { (v, muts) => v -> (muts + (v -> union(muts.getOrElse(v, False), y))) }
          // for (v, spp) <- mutsL do if !branches.contains(v) then branches = branches.updated(v, Map(v -> union(spp, y)))
          // val muts = mutsL
          // val id = union(idL, y)
          // TestMut(xL, branches, muts, id)
        else union(y, x)
      case _ => union(y, x)
    }

  def unionMaps(xs: Iterable[HashMap[Val, SPP]]): HashMap[Val, SPP] =
    // println(xs.map(_.size))
    if xs.isEmpty then return HashMap.empty
    xs.reduce(unionMap)
    // var m = HashMap[Val, SPP]()
    // for x <- xs; (v, spp) <- x do
    //   if m.contains(v) then m = m.updated(v, union(m(v), spp))
    //   else m = m.updated(v, spp)
    // m

  def unionMap(xs: HashMap[Val, SPP], ys: HashMap[Val, SPP]): HashMap[Val, SPP] =
    if ys.isEmpty then return xs
    if xs.isEmpty then return ys
    // println(s"unionMap(${xs.size}, ${ys.size})")
    // var m = scala.collection.mutable.Map.from(xs)
    var m = xs
    for (v, spp) <- ys do
      if m.contains(v) then m = m.updated(v, union(m(v), spp))
      else m = m.updated(v, spp)
    m

  lazy val seq: (SPP, SPP) => SPP = memoize2 { (x, y) => seqPrim(x, y) }
  def seqPrim(x: SPP, y: SPP): SPP =
    (x, y) match {
      case (False, _) => False
      case (_, False) => False
      case (Diag, _) => y
      case (_, Diag) => x
      case (TestMut(xL, branchesL, mutsL, idL), TestMut(xR, branchesR, mutsR, idR)) =>
        if xL == xR then
          // FIXME: the following fastpath is buggy. Doesn't matter much for speed anyway.
          // if branchesL.isEmpty && (idL eq False) && (idR eq False) && mutsR.isEmpty && branchesR.size == 1 then
          //   val (v, muts) = branchesR.head
          //   val spp = mutsL.getOrElse(v, False)
          //   return TestMut(xL, branchesL, muts.map { (v2, spp2) => v2 -> seq(spp, spp2) }, idR)
          // if mutsL.isEmpty && (idL eq False) && (idR eq False) && branchesL.size == 1 then
          //   val (v, muts) = branchesL.head
          //   if muts.size == 1 then
          //     val (v2, spp) = muts.head
          //     if branchesR.contains(v2) then return TestMut(xL, Map(v -> branchesR(v2)), mutsL, idL)
          //     else return TestMut(xL, Map(v -> mutsR), mutsL, idL)

          // logSummary("seq=", x, y)
          val mutsA = unionMaps(mutsL.map { (v2, spp) =>
            get(y, v2).map { (v2, spp2) => v2 -> seq(spp, spp2) }
          })
          val branches = (branchesL.keySet ++ branchesR.keySet ++ mutsL.keySet ++ mutsR.keySet ++ mutsA.keySet)
            .map { v =>
              v -> unionMaps(get(x, v).map { (v2, spp) =>
                get(y, v2).map { (v3, spp2) => v3 -> seq(spp, spp2) }
              })
            }
            .to(HashMap)
          val mutsB = mutsR.map { (v2, spp) => v2 -> seq(idL, spp) }
          SPP.TestMut(xL, branches, unionMap(mutsA, mutsB), seq(idL, idR))
        else if xL < xR then
          seq(x, new TestMut(xL, HashMap.empty, HashMap.empty, y))
          // if mutsL.isEmpty && (idL eq False) then return SPP.TestMut(xL, branchesL.map { (v, muts) => v -> muts.map { (v2, spp) => v2 -> seq(spp, y) } }, mutsL, idL)
          // val mutsA = mutsL.map { (v2, spp) =>
          //   v2 -> seq(spp, y)
          // }
          // if branchesL.isEmpty && (idL eq False) then return TestMut(xL, branchesL, mutsA, idL)
          // // logSummary("seq<", x, y)
          // val branches = (branchesL.keySet ++ mutsL.keySet).map { v =>
          //   v -> unionMaps(get(x, v).map { (v2, spp) => Map(v2 -> seq(spp, y)) })
          // }.to(HashMap)
          // SPP.TestMut(xL, branches, mutsA, seq(idL, y))
        else seq(new TestMut(xR, HashMap.empty, HashMap.empty, x), y)
      // val mutsB = mutsR.map { (v2, spp) => v2 -> seq(x, spp) }
      // if branchesR.isEmpty && (idR eq False) then return SPP.TestMut(xR, branchesR, mutsB, idR)
      // // if branchesR.isEmpty && (idR eq False) then return SPP.TestMut(xR, branchesR, mutsB, idR)
      // // logSummary("seq>", x, y)
      // val branches = (branchesR.keySet ++ mutsR.keySet).map { v =>
      //   v -> get(y, v).map { (v3, spp2) => v3 -> seq(x, spp2) }
      // }.to(HashMap)
      // SPP.TestMut(xR, branches, mutsB, seq(x, idR))
    }

  lazy val intersection: (SPP, SPP) => SPP = memoize2 { (x, y) => intersectionPrim(x, y) }
  def intersectionPrim(x: SPP, y: SPP): SPP =
    // logSPP(s"intersection($x, $y)")
    if x eq y then return x
    (x, y) match {
      case (False, _) => False
      case (Diag, TestMut(x, branches, other, id)) =>
        intersection(new TestMut(x, HashMap.empty, HashMap.empty, Diag), y)
      // val branches2 = branches.map { (v, muts) =>
      //   v -> (if muts.contains(v)
      //         then Map(v -> intersection(Diag, muts(v)))
      //         else HashMap.empty)
      // } ++ (other -- branches.keySet).map { (v, spp) => v -> Map(v -> intersection(Diag, spp)) }
      // val id2 = intersection(Diag, id)
      // TestMut(x, branches2, HashMap.empty, id2)
      case (TestMut(xL, branchesL, mutsL, idL), TestMut(xR, branchesR, mutsR, idR)) =>
        if xL == xR then
          // FIXME: potentially inefficient
          val branches = (branchesL.keySet ++ branchesR.keySet ++ mutsL.keySet ++ mutsR.keySet)
            .map { v =>
              v -> intersectionMap(get(x, v), get(y, v))
            }
            .to(HashMap)
          val muts = intersectionMap(mutsL, mutsR)
          val id = intersection(idL, idR)
          TestMut(xL, branches, muts, id)
        else if xL < xR then
          intersection(x, new TestMut(xL, HashMap.empty, HashMap.empty, y))
          // val branches = branchesL.map { (v, muts) =>
          //   v -> (if muts.contains(v) then Map(v -> intersection(muts(v), y)) else HashMap.empty)
          // } ++ (mutsL -- branchesL.keySet).map { (v, spp) => v -> Map(v -> intersection(spp, y)) }
          // val id = intersection(idL, y)
          // TestMut(xL, branches, HashMap.empty, id)
        else intersection(y, x)
      case _ => intersection(y, x)
    }

  def intersectionMap(xs: HashMap[Val, SPP], ys: HashMap[Val, SPP]): HashMap[Val, SPP] =
    (xs.keySet ++ ys.keySet).map { v => v -> intersection(xs.getOrElse(v, False), ys.getOrElse(v, False)) }.to(HashMap)

  lazy val difference: (SPP, SPP) => SPP = memoize2 { (x, y) => differencePrim(x, y) }
  def differencePrim(x: SPP, y: SPP): SPP =
    // logSPP(s"difference($x, $y)")
    if x eq y then return False
    (x, y) match {
      case (False, _) => False
      case (_, False) => x
      case (Diag, TestMut(xR, branchesR, otherR, idR)) => difference(new TestMut(xR, HashMap.empty, HashMap.empty, Diag), y)
      case (TestMut(xL, branchesL, mutsL, idL), Diag) => difference(x, new TestMut(xL, HashMap.empty, HashMap.empty, Diag))
      case (TestMut(xL, branchesL, mutsL, idL), TestMut(xR, branchesR, mutsR, idR)) =>
        if xL == xR then
          val branches = (branchesL.keySet ++ branchesR.keySet ++ mutsL.keySet ++ mutsR.keySet)
            .map { v =>
              v -> differenceMap(get(x, v), get(y, v))
            }
            .to(HashMap)
          val muts = differenceMap(mutsL, mutsR)
          val id = difference(idL, idR)
          TestMut(xL, branches, muts, id)
        else if xL < xR then difference(x, new TestMut(xL, HashMap.empty, HashMap.empty, y))
        else difference(new TestMut(xR, HashMap.empty, HashMap.empty, x), y)
    }

  def differenceMap(xs: HashMap[Val, SPP], ys: HashMap[Val, SPP]): HashMap[Val, SPP] =
    (xs.keySet ++ ys.keySet).map { v => v -> difference(xs.getOrElse(v, False), ys.getOrElse(v, False)) }.to(HashMap)

  def xor(x: SPP, y: SPP): SPP =
    union(difference(x, y), difference(y, x)) // FIXME: optimize this

  // def star(spp: SPP): SPP =
  //   // logSPP(s"star($spp)")
  //   var x: SPP = False
  //   var y: SPP = Diag
  //   while (true) {
  //     val xnew = union(x, y)
  //     if x eq xnew then return x
  //     x = xnew
  //     y = seq(y, spp)
  //   }
  //   return x // dummy to make compiler happy

  def star(spp: SPP): SPP =
    var x = union(spp, Diag)
    while (true) {
      val xnew = seq(x, x)
      if x eq xnew then return x
      else x = xnew
    }
    return x // dummy to make compiler happy

  def equiv(x: SPP, y: SPP): Boolean =
    x eq y

  // TODO: optimize this by not constructing intermediate SPPs
  def equivAt(sp: SP, x: SPP, y: SPP): Boolean =
    // logSPP(s"equivAt($sp, $x, $y)")
    equiv(seqSP(sp, x), seqSP(sp, y))
}
