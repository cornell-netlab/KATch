package nkpl
import scala.collection.immutable.HashMap

/** Represents symbolic packet set
  */
class SP

object SP {

  /** Represents accepting a packet.
    */
  case object True extends SP

  /** Represents a dropping a packet.
    */
  case object False extends SP

  /** Represents an n-ary test on a variable `x`, a map of cases `ys`, and a default case `default`.
    *
    * @param x
    *   The variable to be tested.
    * @param ys
    *   The map of cases. Each case is a value `v` and a sub-policy `sp`.
    * @param default
    *   The default case if none of the values match.
    */
  case class Test(x: Var, ys: HashMap[Val, SP], default: SP) extends SP {
    // Cache the hashcode
    override val hashCode = x.hashCode + ys.hashCode + default.hashCode
  }

  /** Smart constructors for the Test class.
    */
  object Test {
    // Cache to store instances of Test
    val cache = scala.collection.mutable.HashMap.empty[Test, Test]

    /** Creates a new Test instance with the given parameters. If the given ys map contains any entries with the value equal to default, those entries are filtered out. If the resulting ys2 map is empty, the default value is returned. Otherwise, a new Test instance is created with the filtered ys2 map and the default value, and it is either retrieved from the cache or added to the cache.
      */
    def apply(x: Var, ys: HashMap[Val, SP], default: SP) = {
      val ys2 = ys.filterNot { (v, y) => y eq default }
      if ys2.isEmpty then default
      else {
        val sp = new Test(x, ys2, default)
        cache.getOrElseUpdate(sp, sp)
      }
    }

    /** Creates a new Test instance with the given parameters. This skips some of the optimizations in the apply method; the user is responsible for ensuring that the invariant is maintained.
      */
    def mk(x: Var, ys: HashMap[Val, SP], default: SP) = {
      if ys.isEmpty then default
      else {
        val sp = new Test(x, ys, default)
        cache.getOrElseUpdate(sp, sp)
      }
    }
  }

  /** Converts a given SP to a pretty string representation.
    *
    * @param sp
    *   The SP to convert.
    * @return
    *   The pretty string representation of the SP.
    */
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

  /** Represents the union operation on two SP objects. Memoized for efficiency. The `union` function takes two SP objects `x` and `y` and returns their union. The `unionPrim` function is the helper function that performs the actual union operation.
    *
    * @param x
    *   The first SP object.
    * @param y
    *   The second SP object.
    * @return
    *   The union of `x` and `y`.
    */
  lazy val union: (SP, SP) => SP = memoize2 { (x, y) => unionPrim(x, y) }

  /** Helper function that performs the actual union operation on two SP objects.
    */
  def unionPrim(x: SP, y: SP): SP =
    if x eq y then return x
    (x, y) match {
      case (False, _) => y
      case (True, _) => True
      case (Test(xL, ysL, defaultL), Test(xR, ysR, defaultR)) =>
        if xL == xR then
          if (defaultL eq False) && (defaultR eq False) then
            var ys = ysL
            for (v, sp) <- ysR do
              val y = ys.getOrElse(v, null)
              if y != null then ys = ys.updated(v, union(y, sp))
              else ys = ys.updated(v, sp)
            Test(xL, ys, False)
          else
            // println(s"unionPrim ${ysL.size} ${ysR.size} ${defaultL == False} ${defaultR == False}")
            val keys = ysL.keySet ++ ysR.keySet
            val ys = keys.map { v => v -> union(ysL.getOrElse(v, defaultL), ysR.getOrElse(v, defaultR)) }.to(HashMap)
            Test(xL, ys, union(defaultL, defaultR))
        else if xL < xR then Test(xL, ysL.map { (v, a) => v -> union(a, y) }, union(defaultL, y))
        else union(y, x)
      case _ => union(y, x)
    }

  /** Takes an iterable collection of SP objects and performs a union operation on them.
    *
    * @param xs
    *   The iterable collection of SP objects.
    * @return
    *   The result of the union operation.
    */
  def unionN(xs: Iterable[SP]): SP =
    xs.foldLeft(False: SP)(union(_, _))

  /** Negates a given SP.
    *
    * @param x
    *   The SP to be negated.
    * @return
    *   The negated SP.
    */
  lazy val negate: SP => SP = memoize { x => negatePrim(x) }
  def negatePrim(x: SP): SP =
    x match {
      case False => True
      case True => False
      case Test(x, ys, default) => Test(x, ys.map { (v, a) => v -> negate(a) }, negate(default))
    }

  /** Calculates the difference between two symbolic packets (SP).
    *
    * @param x
    *   The first symbolic packet.
    * @param y
    *   The second symbolic packet.
    * @return
    *   The difference between the two symbolic packets.
    */
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

  /** Calculates the intersection of two SPs.
    *
    * @param x
    *   The first SP object.
    * @param y
    *   The second SP object.
    * @return
    *   The intersection of x and y.
    */
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

  /** Computes the exclusive OR (XOR) of two SP objects.
    *
    * @param x
    *   The first SP object.
    * @param y
    *   The second SP object.
    * @return
    *   The result of the XOR operation.
    */
  def xor(x: SP, y: SP): SP =
    union(difference(x, y), difference(y, x))

  /** Calculates the intersection of a collection of SP objects.
    *
    * @param xs
    *   The collection of SP objects to intersect.
    * @return
    *   The intersection of the SP objects in the collection.
    */
  def intersectionN(xs: Iterable[SP]): SP =
    xs.foldLeft(True: SP)(intersection(_, _))

  /** Checks if a given packet satisfies a given SP.
    *
    * @param packet
    *   The packet to be checked, represented as a map of variable-value pairs.
    * @param x
    *   The SP to be checked against the packet.
    * @return
    *   `true` if the packet satisfies the SP, `false` otherwise.
    */
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

  /** Removes one variable `x` by checking if a packet with any value for `x` exists in the given SP `sp`.
    *
    * @param x
    *   The variable to remove.
    * @param sp
    *   The SP to remove the variable from.
    * @return
    *   The SP with the variable `x` removed.
    */
  lazy val exists: (Var, SP) => SP = memoize2 { (x, sp) => existsPrim(x, sp) }
  def existsPrim(x: Var, sp: SP): SP =
    sp match {
      case SP.False => False
      case SP.True => True
      case SP.Test(y, ys, default) =>
        if x == y then union(unionN(ys.values), default)
        else Test(y, ys.map { (v, sp) => v -> exists(x, sp) }, exists(x, default))
    }

  /** Removes one variable `x` by checking if for every value of `x`, a packets with that value exists in the given SP `sp`.
    *
    * @param x
    *   The variable to remove.
    * @param sp
    *   The SP to remove the variable from.
    * @return
    *   The SP with the variable `x` removed.
    */
  lazy val forall: (Var, SP) => SP = memoize2 { (x, sp) => forallPrim(x, sp) }
  def forallPrim(x: Var, sp: SP): SP =
    sp match {
      case SP.False => True
      case SP.True => True
      case SP.Test(y, ys, default) =>
        if x == y then intersection(intersectionN(ys.values), default)
        else Test(y, ys.map { (v, sp) => v -> forall(x, sp) }, forall(x, default))
    }

  /** Creates a test SP with the given variable and value.
    * @param x
    *   The variable to test.
    * @param y
    *   The value to compare against.
    * @return
    *   The test SP.
    */
  def test(x: Var, y: Val): SP = Test(x, HashMap(y -> True), False)

  /** Creates a negated test SP with the given variable and value.
    * @param x
    *   The variable to test.
    * @param y
    *   The value to compare against.
    * @return
    *   The negated test SP.
    */
  def testNE(x: Var, y: Val): SP = Test(x, HashMap(y -> False), True)
}

/** Represents a Symbolic Packet Program (SPP). This is a symbolic representation of a dup-free NetKAT expression.
  */
class SPP

object SPP {

  /** Represents a symbolic packet program that doesn't change the packet.
    */
  case object Diag extends SPP

  /** Represents the False case in SPP. This case indicates that the packet should be dropped.
    */
  case object False extends SPP

  /** Represents a test and mutation in SPP. This case indicates that the packet should be tested and possibly mutated.
    *
    * @param x
    *   The variable to be tested.
    * @param branches
    *   The map of cases. Each case is a value `v` and a map of mutations.
    * @param other
    *   The map of default mutations if the value of `x` is not in branches.
    * @param id
    *   The default case if none of the values match branches, and the value is not mutated.
    */
  case class TestMut(x: Var, branches: HashMap[Val, HashMap[Val, SPP]], other: HashMap[Val, SPP], id: SPP) extends SPP {
    // Cache the hashcode
    override val hashCode = {
      // println(s"${branches.size} ${other.size}");
      x.hashCode + branches.hashCode + other.hashCode + id.hashCode
    }
  }
  object TestMut {
    val cache = scala.collection.mutable.HashMap.empty[TestMut, TestMut]

    /** Smart constructor for the TestMut class. This constructor removes redundant branches. It also removes mutations that are False from branches and other. The function uses a cache to store instances of TestMut.
      */
    def apply(x: Var, branches: HashMap[Val, HashMap[Val, SPP]], other: HashMap[Val, SPP], id: SPP): SPP =
      // Remove redundant branches
      // A branch is redundant if sending the value to other/id would do the same thing

      var branches2 = branches

      // Now we remove muts that are False from other
      // We have to be careful here, because removing a False mut from other may
      // cause a packet to go to id instead of other.
      for (v, spp) <- other do
        if (spp eq False) && !branches2.contains(v)
        then
          val muts = other + (v -> id)
          branches2 = branches2.updated(v, muts)
      val other2 = other.filterNot { (v, spp) => spp eq False }

      // Remove muts that are False from branches
      branches2 = branches.map { (v, muts) => v -> muts.filterNot { (v2, spp) => spp eq False } }

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

    /** Creates a new TestMut instance with the given parameters. This skips some of the optimizations in the apply method; the user is responsible for ensuring that the invariant is maintained.
      */
    def mk(x: Var, branches: HashMap[Val, HashMap[Val, SPP]], other: HashMap[Val, SPP], id: SPP): SPP =
      val v = new TestMut(x, branches, other, id)
      cache.getOrElseUpdate(v, v)
  }

  /** Runs a packet through an SPP and returns the resulting packets.
    *
    * @param p
    *   The packet to run through the SPP.
    * @param spp
    *   The SPP to run the packet through.
    * @return
    *   The resulting packets.
    */
  def run1(p: Map[Var, Val], spp: SPP): Set[Map[Var, Val]] =
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

  /** A test SPP that matches a packet with a given variable and value.
    *
    * @param x
    *   The variable to test.
    * @param y
    *   The value to compare against.
    * @return
    *   The test SPP.
    */
  def test(x: Var, y: Val): SPP =
    TestMut(x, HashMap(y -> HashMap(y -> Diag)), HashMap.empty, False)

  /** A negated test SPP that matches a packet with a given variable and value.
    *
    * @param x
    *   The variable to test.
    * @param y
    *   The value to compare against.
    * @return
    *   The negated test SPP.
    */
  def testNE(x: Var, y: Val): SPP =
    TestMut(x, HashMap(y -> HashMap.empty), HashMap.empty, Diag)

  /** A mutation of a given packet field to a given value.
    *
    * @param x
    *   The variable to mutate.
    * @param y
    *   The value to mutate to.
    * @return
    *   The mutation SPP.
    */
  def mut(x: Var, y: Val): SPP =
    TestMut(x, HashMap.empty, HashMap(y -> Diag), False)

  /** Log a summary of the given SP/SPP. Useful for debugging.
    */
  def logSummarySP(msg: String, sp: SP, spp: SPP): Unit =
    val spstr = sp match {
      case SP.False => "F"
      case SP.True => "T"
      case SP.Test(x, ys, default) =>
        val ls = ys.size
        val lm = if default eq SP.False then "F" else if default eq SP.True then "T" else "?"
        s"(${VarMap(x)},$ls,$lm)"
    }
    val sppstr = spp match {
      case Diag => "D"
      case False => "F"
      case TestMut(x, branches, other, id) =>
        val ls = branches.size
        val lls = branches.map { (_, a) => a.size }.sum
        val lm = other.size
        val idls = if id eq False then "F" else if id eq Diag then "D" else "?"
        s"(${VarMap(x)},$ls/$lls,$lm,$idls)"
    }
    println(s"$msg $spstr, $sppstr")

  /** Runs a symbolic packet through an SPP and returns the resulting symbolic packet.
    *
    * @param packet
    *   The symbolic packet to be processed.
    * @return
    *   The symbolic packet that results from processing the input packet.
    */
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
            // logSummarySP("run/fast", sp, spp)
            var newbranches = HashMap.empty[Val, SP]
            for (v, sp) <- ys do
              val muts = branches.getOrElse(v, null)
              if muts != null then
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

  /** Converts an SP to an SPP.
    *
    * @param sp
    *   The SP to convert.
    * @return
    *   The converted SPP.
    */
  def fromSP(sp: SP): SPP =
    sp match {
      case SP.False => False
      case SP.True => Diag
      case SP.Test(x, ys, default) =>
        TestMut(x, ys.map { (v, sp) => v -> HashMap(v -> fromSP(sp)) }, HashMap.empty, fromSP(default))
    }

  /** Converts a given SPP to an SP by computing the set of packets the SPP can produce.
    *
    * @param spp
    *   The SPP to convert.
    * @return
    *   The corresponding SP.
    */
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

  /** This code block represents an alternative implementation of the `SPP.run` method. While `SPP.run` is more efficient, this implementation is easier to understand.
    */
  lazy val push: (SPP, SP) => SP = memoize2 { (spp, sp) => pushPrim(spp, sp) }
  def pushPrim(spp: SPP, sp: SP): SP =
    toSPforward(seq(fromSP(sp), spp))

  /** Converts an SPP to an SP by computing the set of packets that can produce an output when run through the SPP
    *
    * @param spp
    *   The SPP to convert.
    * @return
    *   The corresponding SP.
    */
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

  /** Runs a symbolic packet through an SPP backward and returns the symbolic input packet that results. This method aims to find all input packets that can produce the given output packet.
    *
    * @return
    *   The symbolic input packet that results in the given output packet being produced.
    */
  lazy val pull: (SPP, SP) => SP = memoize2 { (spp, sp) => pullPrim(spp, sp) }
  def pullPrim(spp: SPP, sp: SP): SP =
    toSPbackward(seq(spp, fromSP(sp)))

  /** Takes an Iterable of Maps and returns a Map that represents the union of all the Maps. If a key is present in multiple Maps, the corresponding values are merged using the SP.union method.
    *
    * @param xs
    *   The Iterable of Maps to be unioned.
    * @return
    *   The resulting Map after unioning all the Maps.
    */
  def unionMapsSP(xs: Iterable[Map[Val, SP]]): Map[Val, SP] =
    var m = scala.collection.mutable.Map[Val, SP]()
    for x <- xs; (v, sp) <- x do
      if m.contains(v) then m(v) = SP.union(m(v), sp)
      else m(v) = sp
    m.to(HashMap)

  /** Combines two maps of values and corresponding SPs into a single map. If a value exists in both maps, the corresponding SPs are combined using the `SP.union` method. If a value exists in only one map, it is added to the resulting map as is.
    *
    * @param xs
    *   The first map of values to SPs.
    * @param ys
    *   The second map of values to SPs.
    * @return
    *   A new map containing the combined values and SPs from both input maps.
    */
  def unionMapSP(xs: Map[Val, SP], ys: Map[Val, SP]): Map[Val, SP] =
    var m = scala.collection.mutable.Map.from(xs)
    for (v, sp) <- ys do
      if m.contains(v) then m(v) = SP.union(m(v), sp)
      else m(v) = sp
    m.to(HashMap)

  /** Converts an SP to an SPP by interpreting it as a test.
    *
    * @param sp
    *   The SP to convert.
    * @return
    *   The converted SPP.
    */
  def toTest(sp: SP): SPP =
    sp match
      case SP.False => SPP.False
      case SP.True => SPP.Diag
      case SP.Test(x, ys, default) =>
        SPP.TestMut(x, ys.map { (v, sp) => v -> HashMap(v -> toTest(sp)) }, HashMap.empty, toTest(default))

  /** Composes a test represented as a SP with a SPP. We could implement this by converting the SP to a SPP and then composing the SPPs, but this is more efficient.
    *
    * @param sp
    *   The test represented as a SP.
    * @param spp
    *   The SPP to be composed with the SP.
    * @return
    *   The composed test as a SPP.
    */
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
        else if x < x2 then seqSP(sp, new TestMut(x, HashMap.empty, HashMap.empty, spp))
        else seqSP(new SP.Test(x2, HashMap.empty, sp), spp)
    }

  /** Gets a representation of what happens to a packet where the top level variable has value `v`.
    *
    * @param x
    *   The SPP to get the representation from.
    * @param v
    *   The value of the top level variable.
    * @return
    *   The representation of what happens to a packet where the top level variable has value `v`.
    */
  def get(x: SPP, v: Val): HashMap[Val, SPP] =
    x match {
      case TestMut(x, branches, other, id) =>
        if branches.contains(v) then branches(v)
        else if other.contains(v) || (id eq False) then other
        else other + (v -> id)
      case False => assert { false }
      case Diag => assert { false }
    }

  /** Print a summary of the given SPPs. Useful for debugging.
    */
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

  /** Computes the union of two SPPs.
    *
    * @param x
    *   The first SPP.
    * @param y
    *   The second SPP.
    * @return
    *   The union of the two SPPs.
    */
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

  /** Takes an Iterable of HashMaps and returns a single HashMap that is the union of all the input HashMaps.
    *
    * @param xs
    *   The Iterable of HashMaps to be unioned.
    * @return
    *   The union of all the input HashMaps.
    */
  def unionMaps(xs: Iterable[HashMap[Val, SPP]]): HashMap[Val, SPP] =
    if xs.isEmpty then return HashMap.empty
    xs.reduce(unionMap)

  /** Takes two HashMaps of type [Val, SPP] and returns a new HashMap that represents the union of the two input HashMaps. If either of the input HashMaps is empty, the non-empty HashMap is returned as the result. If a key exists in both input HashMaps, the corresponding values are combined using the `union` function. If a key exists only in one of the input HashMaps, the corresponding value is added to the result HashMap as is.
    *
    * @param xs
    *   The first input HashMap.
    * @param ys
    *   The second input HashMap.
    * @return
    *   A new HashMap representing the union of the input HashMaps.
    */
  def unionMap(xs: HashMap[Val, SPP], ys: HashMap[Val, SPP]): HashMap[Val, SPP] =
    if ys.isEmpty then return xs
    if xs.isEmpty then return ys
    var m = xs
    for (v, spp) <- ys do
      if m.contains(v) then m = m.updated(v, union(m(v), spp))
      else m = m.updated(v, spp)
    m

  /** Sequential composition of two SPPs.
    *
    * @param x
    *   the first SPP
    * @param y
    *   the second SPP
    * @return
    *   the result of the sequential composition
    */
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

  /** Calculates the intersection of two SPPs.
    *
    * @param x
    *   The first SPP.
    * @param y
    *   The second SPP.
    * @return
    *   The intersection of the two SPPs.
    */
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

  /** Computes the intersection of two maps of SPPs, where the keys are of type Val and the values are of type SPP. If a key is present in both HashMaps, the intersection of the corresponding values is computed. If a key is present in only one HashMap, the corresponding value is treated as False.
    *
    * @param xs
    *   The first HashMap.
    * @param ys
    *   The second HashMap.
    * @return
    *   A new HashMap containing the intersection of xs and ys.
    */
  def intersectionMap(xs: HashMap[Val, SPP], ys: HashMap[Val, SPP]): HashMap[Val, SPP] =
    (xs.keySet ++ ys.keySet).map { v => v -> intersection(xs.getOrElse(v, False), ys.getOrElse(v, False)) }.to(HashMap)

  /** Calculates the difference of two SPPs.
    *
    * @param x
    *   The first SPP.
    * @param y
    *   The second SPP.
    * @return
    *   The difference of the two SPPs.
    */
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

  /** Computes the difference between two HashMaps of Val and SPP.
    *
    * @param xs
    *   The first HashMap.
    * @param ys
    *   The second HashMap.
    * @return
    *   A new HashMap containing the difference between xs and ys.
    */
  def differenceMap(xs: HashMap[Val, SPP], ys: HashMap[Val, SPP]): HashMap[Val, SPP] =
    (xs.keySet ++ ys.keySet).map { v => v -> difference(xs.getOrElse(v, False), ys.getOrElse(v, False)) }.to(HashMap)

  /** Computes the exclusive OR (XOR) of two SPPs.
    *
    * @param x
    *   The first SPP.
    * @param y
    *   The second SPP.
    * @return
    *   The XOR of the two SPPs.
    */
  def xor(x: SPP, y: SPP): SPP =
    union(difference(x, y), difference(y, x)) // FIXME: optimize this

  /** Returns a new instance of `SPP` that represents the star operation on the given `spp`. The star represents repetition of the given `spp` zero or more times. The implementation uses repeated squaring to compute the star operation efficiently.
    *
    * @param spp
    *   The `SPP` object to apply the star operation on.
    * @return
    *   A new instance of `SPP` representing the star operation on `spp`.
    */
  def star(spp: SPP): SPP =
    var x = union(spp, Diag)
    while (true) {
      val xnew = seq(x, x)
      if x eq xnew then return x
      else x = xnew
    }
    return x // dummy to make compiler happy

  /** Checks if two SPP objects are equivalent. This is the same as object identity.
    *
    * @param x
    *   The first SPP object.
    * @param y
    *   The second SPP object.
    * @return
    *   True if the two SPP objects are equivalent, false otherwise.
    */
  def equiv(x: SPP, y: SPP): Boolean =
    x eq y

  /** Checks if two SPPs are equivalent under a given SP input packets.
    *
    * @param sp
    *   The SP to evaluate the equivalence under.
    * @param x
    *   The first SPP.
    * @param y
    *   The second SPP.
    * @return
    *   True if the two SPPs are equivalent, false otherwise.
    */
  def equivAt(sp: SP, x: SPP, y: SPP): Boolean =
    equiv(seqSP(sp, x), seqSP(sp, y))

  /** Converts an instance of SPP to an SP if the SPP doesn't do any mutation. Otherwise, returns None.
    *
    * @param spp
    *   The SPP instance to convert.
    * @return
    *   An optional SP instance if the conversion is successful, otherwise None.
    */
  lazy val toSP: SPP => Option[SP] = memoize { spp => toSPprim(spp) }
  def toSPprim(spp: SPP): Option[SP] =
    spp match {
      case False => Some(SP.False)
      case Diag => Some(SP.True)
      case TestMut(x, branches, other, id) =>
        var failed = false
        // convert id
        val id2 = toSP(id) match {
          case None => { failed = true; SP.False }
          case Some(sp) => sp
        }
        if !other.isEmpty then failed = true
        // convert branches
        val branches2 = branches
          .map { (v, muts) =>
            if muts.size == 1 && muts.contains(v) then
              toSP(muts(v)) match {
                case None => { failed = true; (v, SP.False) }
                case Some(sp) => (v, sp)
              }
            else if muts.size == 0 then (v, SP.False)
            else { failed = true; (v, SP.False) }
          }
          .to(HashMap)
        if failed then None
        else Some(SP.Test(x, branches2, id2))
    }
}
