package nkpl

class SP

object SP {
  case object True extends SP
  case object False extends SP
  case class Test(x: Var, ys: Map[Val, SP], default: SP) extends SP

  object Test {
    private val cache = scala.collection.mutable.WeakHashMap.empty[Test, Test]
    def apply(x: Var, ys: Map[Val, SP], default: SP) =
      val ys2 = ys.filterNot { (v, y) => y eq default }
      if ys2.isEmpty then default
      else
        val sp = new Test(x, ys2, default)
        cache.getOrElseUpdate(sp, sp)
  }

  def union(x: SP, y: SP): SP =
    (x, y) match {
      case (False, _) => y
      case (True, _) => True
      case (Test(xL, ysL, defaultL), Test(xR, ysR, defaultR)) =>
        if xL == xR then
          val keys = ysL.keySet ++ ysR.keySet
          val ys = keys.map { v => v -> union(ysL.getOrElse(v, defaultL), ysR.getOrElse(v, defaultR)) }.toMap
          Test(xL, ys, union(defaultL, defaultR))
        else if xL < xR then Test(xL, ysL.map { (v, a) => v -> union(a, y) }, union(defaultL, y))
        else union(y, x)
      case _ => union(y, x)
    }

  def unionN(xs: Iterable[SP]): SP =
    xs.foldLeft(False: SP)(union(_, _))

  def negate(x: SP): SP =
    x match {
      case False => True
      case True => False
      case Test(x, ys, default) => Test(x, ys.map { (v, a) => v -> negate(a) }, negate(default))
    }

  def difference(x: SP, y: SP): SP =
    (x, y) match {
      case (False, _) => False
      case (_, False) => x
      case (_, True) => False
      case (True, _) => negate(y)
      case (Test(xL, ysL, defaultL), Test(xR, ysR, defaultR)) =>
        if xL == xR then
          val keys = ysL.keySet ++ ysR.keySet
          val ys = keys.map { v => v -> difference(ysL.getOrElse(v, defaultL), ysR.getOrElse(v, defaultR)) }.toMap
          Test(xL, ys, difference(defaultL, defaultR))
        else if xL < xR then Test(xL, ysL.map { (v, a) => v -> difference(a, y) }, difference(defaultL, y))
        else Test(xR, ysR.map { (v, a) => v -> difference(x, a) }, difference(x, defaultR))
    }

  def intersection(x: SP, y: SP): SP =
    (x, y) match {
      case (False, _) => False
      case (True, _) => y
      case (Test(xL, ysL, defaultL), Test(xR, ysR, defaultR)) =>
        if xL == xR then
          val keys = ysL.keySet ++ ysR.keySet
          val ys = keys.map { v => v -> intersection(ysL.getOrElse(v, defaultL), ysR.getOrElse(v, defaultR)) }.toMap
          Test(xL, ys, intersection(defaultL, defaultR))
        else if xL < xR then Test(xL, ysL.map { (v, a) => v -> intersection(a, y) }, intersection(defaultL, y))
        else intersection(y, x)
      case _ => intersection(y, x)
    }

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
}

class SPP

object SPP {
  def logSPP(x: String): Unit =
    // println(x)
    ()

  case object Diag extends SPP
  case object False extends SPP
  case class TestMut(x: Var, branches: Map[Val, Map[Val, SPP]], other: Map[Val, SPP], id: SPP) extends SPP
  object TestMut {
    private val cache = scala.collection.mutable.WeakHashMap.empty[TestMut, TestMut]
    def apply(x: Var, branches: Map[Val, Map[Val, SPP]], other: Map[Val, SPP], id: SPP): SPP =
      // Remove redundant branches
      // A branch is redundant if sending the value to other/id would do the same thing

      // First, we remove muts that are False from branches
      var branches2 = branches.map { (v, muts) => v -> muts.filterNot { (v2, spp) => spp eq False } }

      // Now we remove muts that are False from other
      // We have to be careful here, because removing a False mut from other may cause a packet to go to id instead of other.
      for (v, spp) <- other do if (spp eq False) && !branches2.contains(v) then branches2 = branches2.updated(v, Map())
      val other2 = other.filterNot { (v, spp) => spp eq False }

      // for (v, spp) <- other do if (spp eq False) || branches2.contains(v) then branches2 = branches2.updated(v, Map())
      // val other2 = other.filterNot { (v, spp) => spp eq False } // FIXME: is this correct?
      val branches3 = branches2.filter { (v, muts) =>
        muts != (if other2.contains(v) || (id eq False) then other2 else other2 + (v -> id))
      }
      if branches3.isEmpty && other2.isEmpty then return id
      val v = new TestMut(x, branches3, other2, id)
      // val v = new TestMut(x, branches, other, id)
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
    logSPP(s"run1($p, $spp)")
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
    TestMut(x, Map(y -> Map(y -> Diag)), Map(), False)

  def mut(x: Var, y: Val): SPP =
    TestMut(x, Map(), Map(y -> Diag), False)
    // TestMut(x, Map(y -> Map(y -> Diag)), Map(y -> Diag), False)

  // Runs a symbolic packet through an SPP, and returns the symbolic packet that results.
  def run(sp: SP, spp: SPP): SP =
    logSPP(s"run($sp, $spp)")
    (sp, spp) match {
      case (SP.False, _) => SP.False
      case (_, False) => SP.False
      case (_, Diag) => sp
      case (SP.True, TestMut(x, branches, other, id)) =>
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
        SP.Test(x, branchesC.toMap, run(SP.True, id))
      case (SP.Test(x, ys, default), TestMut(x2, branches, other, id)) =>
        if x == x2 then
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
          // Here we need to nest the test for x2 inside the test for x:
          // SP.Test(x, ... SP.Test(x2, ...) ...)
          // This is like the equality case, but with empty branches and other, and id=spp
          SP.Test(x, ys.map { (v, sp2) => v -> run(sp2, spp) }, run(default, spp))
        else
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

  def unionMapsSP(xs: Iterable[Map[Val, SP]]): Map[Val, SP] =
    var m = scala.collection.mutable.Map[Val, SP]()
    for x <- xs; (v, sp) <- x do
      if m.contains(v) then m(v) = SP.union(m(v), sp)
      else m(v) = sp
    m.toMap

  def unionMapSP(xs: Map[Val, SP], ys: Map[Val, SP]): Map[Val, SP] =
    var m = scala.collection.mutable.Map.from(xs)
    for (v, sp) <- ys do
      if m.contains(v) then m(v) = SP.union(m(v), sp)
      else m(v) = sp
    m.toMap

  // Converts a symbolic packet into a test SPP
  def toTest(sp: SP): SPP =
    sp match
      case SP.False => SPP.False
      case SP.True => SPP.Diag
      case SP.Test(x, ys, default) =>
        SPP.TestMut(x, ys.map { (v, sp) => v -> Map(v -> toTest(sp)) }, Map(), toTest(default))

  // Composes a test represented as a SP with a SPP.
  // We could implement this by converting the SP to a SPP and then composing the SPPs.
  def seqSP(sp: SP, spp: SPP): SPP =
    logSPP(s"seqSP($sp, $spp)")
    (sp, spp) match {
      case (SP.False, _) => False
      case (SP.True, _) => spp
      case (_, False) => False
      case (_, Diag) => toTest(sp)
      case (SP.Test(x, ys, default), TestMut(x2, branches, other, id)) =>
        if x == x2 then
          val branches2 = (ys.keySet ++ branches.keySet ++ other.keySet).map { case v =>
            val y = ys.getOrElse(v, default)
            val b = branches.getOrElse(v, if other.contains(v) then other else other + (v -> id))
            v -> b.map { (v2, spp) => v2 -> seqSP(y, spp) }
          }.toMap
          val other2 = other.map { (v2, spp) => v2 -> seqSP(default, spp) }
          val id2 = seqSP(default, id)
          TestMut(x, branches2, other2, id2)
        else if x < x2 then
          seqSP(sp, new TestMut(x, Map(), Map(), spp))
          // val branches2 = ys.map { (v, sp) => v -> Map(v -> seqSP(sp, spp)) }
          // TestMut(x, branches2, Map(), seqSP(default, spp))
        else seqSP(new SP.Test(x2, Map(), sp), spp)
      // val branches2 = branches.map { (v, muts) =>
      //   v -> muts.map { (v2, spp) => v2 -> seqSP(sp, spp) }
      // }
      // val other2 = other.map { (v, spp) => v -> seqSP(sp, spp) }
      // TestMut(x2, branches2, other2, seqSP(sp, id))
    }

  def get(x: SPP, v: Val): Map[Val, SPP] =
    x match {
      case TestMut(x, branches, other, id) =>
        if branches.contains(v) then branches(v)
        else if other.contains(v) || (id eq False) then other
        else other + (v -> id)
      case False => Map()
      case Diag => Map(v -> Diag)
    }

  def union(x: SPP, y: SPP): SPP =
    logSPP(s"union($x, $y)")
    if x == y then return x
    (x, y) match {
      case (False, _) => y
      case (_, False) => x
      case (Diag, TestMut(x, branches, other, id)) =>
        union(new TestMut(x, Map(), Map(), Diag), y)
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
          val branches = (branchesL.keySet ++ branchesR.keySet ++ mutsL.keySet ++ mutsR.keySet).map { v =>
            v -> unionMap(get(x, v), get(y, v))
          }.toMap
          val muts = unionMap(mutsL, mutsR)
          val id = union(idL, idR)
          TestMut(xL, branches, muts, id)
        else if xL < xR then
          union(x, new TestMut(xL, Map(), Map(), y))
          // var branches = branchesL.map { (v, muts) => v -> (muts + (v -> union(muts.getOrElse(v, False), y))) }
          // for (v, spp) <- mutsL do if !branches.contains(v) then branches = branches.updated(v, Map(v -> union(spp, y)))
          // val muts = mutsL
          // val id = union(idL, y)
          // TestMut(xL, branches, muts, id)
        else union(y, x)
      case _ => union(y, x)
    }

  def unionMaps(xs: Iterable[Map[Val, SPP]]): Map[Val, SPP] =
    var m = scala.collection.mutable.Map[Val, SPP]()
    for x <- xs; (v, spp) <- x do
      if m.contains(v) then m(v) = union(m(v), spp)
      else m(v) = spp
    m.toMap

  def unionMap(xs: Map[Val, SPP], ys: Map[Val, SPP]): Map[Val, SPP] =
    var m = scala.collection.mutable.Map.from(xs)
    for (v, spp) <- ys do
      if m.contains(v) then m(v) = union(m(v), spp)
      else m(v) = spp
    m.toMap

  def seq(x: SPP, y: SPP): SPP =
    logSPP(s"seq($x, $y)")
    (x, y) match {
      case (False, _) => False
      case (_, False) => False
      case (Diag, _) => y
      case (_, Diag) => x
      case (TestMut(xL, branchesL, mutsL, idL), TestMut(xR, branchesR, mutsR, idR)) =>
        if xL == xR then
          // There are a quite a few cases here:
          // * The input packet can match one of the branches of the lhs
          //    - If so, the output value might match one of the branches of the rhs
          //    - Or not, and in that case goes to mutsR/idR
          // * The input packet might not match one of the branches of the lhs.
          //    If so, we go to mutsL/idL
          //    - The value set by mutsL might match branchesR or not
          //    - The idL continues through branchesR/mutsR/idR

          // First, we look at branchesL
          // val branchesA = branchesL.map { (v, muts) =>
          //   v -> unionMaps(muts.map { (v2, spp) =>
          //     (if branchesR.contains(v2) then branchesR(v2)
          //      else if mutsR.contains(v2) then mutsR
          //      else mutsR + (v2 -> idR)) .map { (v2, spp2) => v2 -> seq(spp, spp2) }
          //   })
          // }
          // val branchesB = (mutsL.keySet -- branchesL.keySet).map { v =>
          //   v -> unionMaps(mutsL.map { (v2, spp) =>
          //     (if branchesR.contains(v2) then branchesR(v2)
          //      else if mutsR.contains(v2) then mutsR
          //      else mutsR + (v2 -> idR)) .map { (v2, spp2) => v2 -> seq(spp, spp2) }
          //   })
          // }
          // // Second, we look at branchesR, which goes through idL
          // // However, only packets that do not match branchesL nor mutsL go through idL
          // val branchesC = ((branchesR -- branchesL.keySet) -- mutsL.keySet).map { (v, muts) =>
          //   v -> unionMap(
          //     branchesR(v).map { (v2, spp) => v2 -> seq(idL, spp) },
          //     unionMaps(mutsL.map { (v2, spp) =>
          //       (if branchesR.contains(v2) then branchesR(v2)
          //        else if mutsR.contains(v2) then mutsR
          //        else mutsR + (v2 -> idR)) .map { (v2, spp2) => v2 -> seq(spp, spp2) }
          //     })
          //   )
          // }
          val mutsA = unionMaps(mutsL.map { (v2, spp) =>
            get(y, v2).map { (v2, spp2) => v2 -> seq(spp, spp2) }
          })
          val branches = (branchesL.keySet ++ branchesR.keySet ++ mutsL.keySet ++ mutsR.keySet ++ mutsA.keySet).map { v =>
            v -> unionMaps(get(x, v).map { (v2, spp) => get(y, v2).map { (v3, spp2) => v3 -> seq(spp, spp2) } })
          }.toMap
          val mutsB = mutsR.map { (v2, spp) => v2 -> seq(idL, spp) }
          SPP.TestMut(xL, branches, unionMap(mutsA, mutsB), seq(idL, idR))
        else if xL < xR then
          seq(x, new TestMut(xL, Map(), Map(), y))
          // val branches2 = branchesL.map { (v, muts) =>
          //   v -> muts.map { (v2, spp) => v2 -> seq(spp, y) }
          // }
          // val muts2 = mutsL.map { (v2, spp) => v2 -> seq(spp, y) }
          // SPP.TestMut(xL, branches2, muts2, seq(idL, y))
        else seq(new TestMut(xR, Map(), Map(), x), y)
      // val branches2 = branchesR.map { (v, muts) =>
      //   v -> muts.map { (v2, spp) => v2 -> seq(x, spp) }
      // }
      // val muts2 = mutsR.map { (v2, spp) => v2 -> seq(x, spp) }
      // SPP.TestMut(xR, branches2, muts2, seq(x, idR))
    }

  def intersection(x: SPP, y: SPP): SPP =
    logSPP(s"intersection($x, $y)")
    if x eq y then return x
    (x, y) match {
      case (False, _) => False
      case (Diag, TestMut(x, branches, other, id)) =>
        intersection(new TestMut(x, Map(), Map(), Diag), y)
      // val branches2 = branches.map { (v, muts) =>
      //   v -> (if muts.contains(v)
      //         then Map(v -> intersection(Diag, muts(v)))
      //         else Map())
      // } ++ (other -- branches.keySet).map { (v, spp) => v -> Map(v -> intersection(Diag, spp)) }
      // val id2 = intersection(Diag, id)
      // TestMut(x, branches2, Map(), id2)
      case (TestMut(xL, branchesL, mutsL, idL), TestMut(xR, branchesR, mutsR, idR)) =>
        if xL == xR then
          // FIXME: potentially inefficient
          val branches = (branchesL.keySet ++ branchesR.keySet ++ mutsL.keySet ++ mutsR.keySet).map { v =>
            v -> intersectionMap(get(x, v), get(y, v))
          }.toMap
          val muts = intersectionMap(mutsL, mutsR)
          val id = intersection(idL, idR)
          TestMut(xL, branches, muts, id)
        else if xL < xR then
          intersection(x, new TestMut(xL, Map(), Map(), y))
          // val branches = branchesL.map { (v, muts) =>
          //   v -> (if muts.contains(v) then Map(v -> intersection(muts(v), y)) else Map())
          // } ++ (mutsL -- branchesL.keySet).map { (v, spp) => v -> Map(v -> intersection(spp, y)) }
          // val id = intersection(idL, y)
          // TestMut(xL, branches, Map(), id)
        else intersection(y, x)
      case _ => intersection(y, x)
    }

  def intersectionMap(xs: Map[Val, SPP], ys: Map[Val, SPP]): Map[Val, SPP] =
    (xs.keySet ++ ys.keySet).map { v => v -> intersection(xs.getOrElse(v, False), ys.getOrElse(v, False)) }.toMap

  def difference(x: SPP, y: SPP): SPP =
    logSPP(s"difference($x, $y)")
    if x eq y then return False
    (x, y) match {
      case (False, _) => False
      case (_, False) => x
      case (Diag, TestMut(xR, branchesR, otherR, idR)) => difference(new TestMut(xR, Map(), Map(), Diag), y)
      case (TestMut(xL, branchesL, mutsL, idL), Diag) => difference(x, new TestMut(xL, Map(), Map(), Diag))
      case (TestMut(xL, branchesL, mutsL, idL), TestMut(xR, branchesR, mutsR, idR)) =>
        if xL == xR then
          val branches = (branchesL.keySet ++ branchesR.keySet ++ mutsL.keySet ++ mutsR.keySet).map { v =>
            v -> differenceMap(get(x, v), get(y, v))
          }.toMap
          val muts = differenceMap(mutsL, mutsR)
          val id = difference(idL, idR)
          TestMut(xL, branches, muts, id)
        else if xL < xR then difference(x, new TestMut(xL, Map(), Map(), y))
        else difference(new TestMut(xR, Map(), Map(), x), y)
    }

  def differenceMap(xs: Map[Val, SPP], ys: Map[Val, SPP]): Map[Val, SPP] =
    (xs.keySet ++ ys.keySet).map { v => v -> difference(xs.getOrElse(v, False), ys.getOrElse(v, False)) }.toMap

  def star(spp: SPP): SPP =
    logSPP(s"star($spp)")
    var x: SPP = False
    var y: SPP = Diag
    while (true) {
      val xnew = union(x, y)
      if x eq xnew then return x
      x = xnew
      y = seq(y, spp)
    }
    return x // dummy to make compiler happy

  def equiv(x: SPP, y: SPP): Boolean =
    x eq y

  // TODO: optimize this by not constructing intermediate SPPs
  def equivAt(sp: SP, x: SPP, y: SPP): Boolean =
    logSPP(s"equivAt($sp, $x, $y)")
    equiv(seqSP(sp, x), seqSP(sp, y))
}
