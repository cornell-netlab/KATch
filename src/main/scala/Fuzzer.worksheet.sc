import nkpl._
import scala.collection.immutable.HashMap

/** Controls the size of the mutations/tests */
val n = 50

/** Tester class to generate random packets, SPs and SPPs
  * @param vals
  *   A map from variables to their possible values
  * @param random
  *   If true, generate random SPs and SPPs
  */
class Tester(vals: Map[Var, Set[Val]], random: Boolean = false):
  val sortedVals = vals.toList.sortBy { (v, s) => v }
  val packets =
    if false then (1 to n).map { _ => randPacket() }.toSet
    else genComb(vals.map { (v, s) => v -> (s ++ Set(-1)) }.to(HashMap)).toList
  val sps =
    if random then (1 to n).map { _ => randSP(randSublist(sortedVals)) }.toList.sortBy(sizeSP)
    else genSP(sortedVals).toList.sortBy(sizeSP)
  val spps =
    if random then (1 to n).map { _ => randSPP(randSublist(sortedVals)) }.toList.sortBy(sizeSPP)
    else genSPP(sortedVals).toList.sortBy(sizeSPP)
  val spps_nocanon =
    if random then (1 to n).map { _ => randSPPnocanon(randSublist(sortedVals)) }.toList.sortBy(sizeSPP)
    else genSPPnocanon(sortedVals).toList.sortBy(sizeSPP)

  def randPacket(): Map[Var, Val] =
    vals.map { (v, s) => v -> s.toList(util.Random.nextInt(s.size)) }

  def randSubset[A](xs: Set[A]): Set[A] =
    xs.filter { _ => util.Random.nextBoolean() }

  def randSublist[A](xs: List[A]): List[A] =
    xs.filter { _ => util.Random.nextBoolean() }

  def randSP(vals: List[(Var, Set[Val])]): SP =
    if vals.isEmpty then if util.Random.nextBoolean() then SP.False else SP.True
    else
      val (x, vs) = vals.head
      val rest = vals.tail
      val branches = randSubset(vs).map { v => v -> randSP(rest) }.to(HashMap)
      val other = randSP(rest)
      SP.Test(x, branches, other)

  def randSPP(vals: List[(Var, Set[Val])]): SPP =
    if vals.isEmpty then if util.Random.nextBoolean() then SPP.False else SPP.Diag
    else
      val (x, vs) = vals.head
      val rest = vals.tail
      val branches = randSubset(vs)
        .map { v =>
          v -> randSubset(vs).map { v => v -> randSPP(rest) }.to(HashMap)
        }
        .to(HashMap)
      val other = randSubset(vs).map { v => v -> randSPP(rest) }.to(HashMap)
      val id = randSPP(rest)
      SPP.TestMut(x, branches, other, id)

  def randSPPnocanon(vals: List[(Var, Set[Val])]): SPP =
    if vals.isEmpty then if util.Random.nextBoolean() then SPP.False else SPP.Diag
    else
      val (x, vs) = vals.head
      val rest = vals.tail
      val branches = randSubset(vs)
        .map { v =>
          v -> randSubset(vs).map { v => v -> randSPP(rest) }.to(HashMap)
        }
        .to(HashMap)
      val other = randSubset(vs).map { v => v -> randSPP(rest) }.to(HashMap)
      val id = randSPP(rest)
      new SPP.TestMut(x, branches, other, id)

  def genComb[A, B](xs: HashMap[A, Set[B]]): Set[HashMap[A, B]] =
    if xs.isEmpty then return Set(HashMap.empty)
    val (x, ys) = xs.head
    val rest = xs.tail
    val combs = genComb(rest)
    for
      comb <- combs
      y <- ys
    yield comb + (x -> y)

  def subMaps[A, B](xs: HashMap[A, B]): Set[HashMap[A, B]] =
    if (xs.isEmpty) Set(HashMap.empty[A, B])
    else
      val (x, y) = xs.head
      val rest = xs.tail
      val submaps = subMaps(rest)
      for {
        submap <- submaps
        result <- Set(submap, submap + (x -> y))
      } yield result

  def subMapss[A, B](xs: Set[HashMap[A, B]]): Set[HashMap[A, B]] =
    xs.flatMap(subMaps(_))

  // Generate all small SPs on a given set of variables and values
  def genSP(vars: List[(Var, Set[Val])]): Set[SP] =
    if vars.isEmpty then return Set(SP.False, SP.True)
    val (x, vals) = vars.head
    val sps = genSP(vars.tail)
    // We map each val to a SP. We also have a default SP
    for
      default <- sps
      ys <- subMapss(genComb(vals.map { v => v -> sps }.to(HashMap)))
    yield SP.Test(x, ys.to(HashMap), default)

  def sizeSP(sp: SP): Int =
    sp match
      case SP.False => 1
      case SP.True => 1
      case SP.Test(x, branches, other) =>
        1 + branches.values.map(sizeSP).sum + sizeSP(other)

  /** Checks if a given function `f` returns `true` for all combinations of packets and SPs.
    *
    * @param f
    *   The function to be checked.
    * @return
    *   `true` if `f` returns `true` for all combinations of packets and SPs, `false` otherwise.
    */
  def checkSP1(f: (Map[Var, Val], SP) => Boolean): Boolean =
    for
      sp <- sps
      packet <- packets
    do
      if !f(packet, sp) then
        println(s"Failed for $sp and $packet")
        return false
    true

  /** Checks if a given function `f` returns `true` for all combinations of `sp1`, `sp2`, and `packet`.
    *
    * @param f
    *   The function to be checked.
    * @return
    *   `true` if `f` returns `true` for all combinations of `sp1`, `sp2`, and `packet`, `false` otherwise.
    */
  def checkSP2(f: (Map[Var, Val], SP, SP) => Boolean): Boolean =
    for
      sp1 <- sps
      sp2 <- sps
      packet <- packets
    do
      if !f(packet, sp1, sp2) then
        println(s"Failed for $sp1, $sp2 and $packet")
        return false
    true

  def genSPP(vars: List[(Var, Set[Val])]): Set[SPP] =
    if vars.isEmpty then return Set(SPP.False, SPP.Diag)
    val (x, vals) = vars.head
    val spps = genSPP(vars.tail)
    val others = subMapss(genComb(vals.map { v => v -> spps }.to(HashMap)))
    val branchess = subMapss(genComb(vals.map { v => v -> others }.to(HashMap)))
    for
      branches <- branchess
      other <- others
      id <- spps
    yield SPP.TestMut(x, branches.to(HashMap), other.to(HashMap), id)

  def genSPPnocanon(vars: List[(Var, Set[Val])]): Set[SPP] =
    if vars.isEmpty then return Set(SPP.False, SPP.Diag)
    val (x, vals) = vars.head
    val spps = genSPP(vars.tail)
    val others = subMapss(genComb(vals.map { v => v -> spps }.to(HashMap)))
    val branchess = subMapss(genComb(vals.map { v => v -> others }.to(HashMap)))
    for
      branches <- branchess
      other <- others
      id <- spps
    yield new SPP.TestMut(x, branches.to(HashMap), other.to(HashMap), id)

  def sizeSPP(spp: SPP): Int =
    spp match
      case SPP.False => 1
      case SPP.Diag => 2
      case SPP.TestMut(x, branches, other, id) =>
        1 + branches.values.map { muts => muts.values.map(sizeSPP).sum + 1 }.sum + other.values.map(sizeSPP).sum + sizeSPP(id)

  def check_SPP_nocanon(f: SPP => Boolean): Boolean =
    for spp <- spps_nocanon
    do
      if !f(spp) then
        println(s"Failed for $spp")
        return false
    true

  /** Checks if a given function `f` returns `true` for all combinations of `SPP` and `SP` objects.
    *
    * @param f
    *   The function to be checked.
    * @return
    *   `true` if `f` returns `true` for all combinations of `SPP` and `SP` objects, `false` otherwise.
    */
  def check_SPP_SP(f: (SPP, SP) => Boolean): Boolean =
    for
      spp <- spps
      sp <- sps
    do
      if !f(spp, sp) then
        println(s"Failed for \n val sp = $sp \n val spp = $spp")
        return false
    true

  /** Checks if a given function `f` returns `true` for all combinations of `SPP` objects.
    *
    * @param f
    *   The function to be checked.
    * @return
    *   `true` if `f` returns `true` for all combinations of `SPP` objects, `false` otherwise.
    */
  def check_SPP_SPP(f: (SPP, SPP) => Boolean): Boolean =
    for
      spp1 <- spps
      spp2 <- spps
    do
      if !f(spp1, spp2) then
        println(s"Failed for \n val spp1 = $spp1 \n val spp2 = $spp2")
        return false
    true

  /** Checks if the given function `f` returns `true` for all combinations of `SPP` objects and `packet` objects.
    *
    * @param f
    *   The function to be checked.
    * @return
    *   `true` if `f` returns `true` for all combinations, `false` otherwise.
    */
  def check_SPP_SPP_P(f: (SPP, SPP, Map[Var, Val]) => Boolean): Boolean =
    for
      spp1 <- spps
      spp2 <- spps
      packet <- packets
    do
      if !f(spp1, spp2, packet) then
        println(s"Failed for \n val spp1 = $spp1 \n val spp2 = $spp2 \n val packet = $packet")
        return false
    true

  /** Checks if a given function `f` returns `true` for all combinations of `SPP`, `SP`, and `packet`.
    *
    * @param f
    *   The function to be checked.
    * @return
    *   `true` if `f` returns `true` for all combinations of `SPP`, `SP`, and `packet`, `false` otherwise.
    */
  def check_SPP_SP_P(f: (SPP, SP, Map[Var, Val]) => Boolean): Boolean =
    for
      spp <- spps
      sp <- sps
      packet <- packets
    do
      if !f(spp, sp, packet) then
        println(s"Failed for \n val spp = $spp \n val sp = $sp \n val packet = $packet")
        return false
    true

  /** Checks if a given function `f` returns `true` for all combinations of `SPP`, `SP`, and packets.
    *
    * @param f
    *   The function to be checked.
    * @return
    *   `true` if `f` returns `true` for all combinations, `false` otherwise.
    */
  def check_SPP_SP_P_P(f: (SPP, SP, Map[Var, Val], Map[Var, Val]) => Boolean): Boolean =
    for
      spp <- spps
      sp <- sps
      packet1 <- packets
      packet2 <- packets
    do
      if !f(spp, sp, packet1, packet2) then
        println(s"Failed for \n val spp = $spp \n val sp = $sp \n val packet1 = $packet1 \n val packet2 = $packet2")
        return false
    true

  /** Checks if the given function `f` returns `true` for all combinations of `SP` objects.
    *
    * @param f
    *   The function to be checked.
    * @return
    *   `true` if `f` returns `true` for all combinations of `SP` objects, `false` otherwise.
    */
  def check_SP_SP(f: (SP, SP) => Boolean): Boolean =
    for
      sp1 <- sps
      sp2 <- sps
    do
      if !f(sp1, sp2) then
        println(s"Failed for \n val sp1 = $sp1 \n val sp2 = $sp2")
        return false
    true

  /** Checks if a given function `f` returns `true` for all combinations of `SP`, `SP`, and packets.
    *
    * @param f
    *   The function to be checked.
    * @return
    *   `true` if `f` returns `true` for all combinations, `false` otherwise.
    */
  def check_SP_SP_P(f: (SP, SP, Map[Var, Val]) => Boolean): Boolean =
    for
      sp1 <- sps
      sp2 <- sps
      packet <- packets
    do
      if !f(sp1, sp2, packet) then
        println(s"Failed for \n val sp1 = $sp1 \n val sp2 = $sp2 \n val packet = $packet")
        return false
    true

  /** Checks if a given function `f` returns `true` for all combinations of `SP` and packets.
    *
    * @param f
    *   The function to be checked.
    * @return
    *   `true` if `f` returns `true` for all combinations of `SP` and packets, `false` otherwise.
    */
  def check_SP_P(f: (SP, Map[Var, Val]) => Boolean): Boolean =
    for
      sp <- sps
      packet <- packets
    do
      if !f(sp, packet) then
        println(s"Failed for \n val sp = $sp \n val packet = $packet")
        return false
    true

  /** Checks if a given function `f` returns `true` for all combinations of `SPP` and packets.
    *
    * @param f
    *   The function to be checked.
    * @return
    *   `true` if `f` returns `true` for all combinations of `SPP` and `packet`, `false` otherwise.
    */
  def check_SPP_P(f: (SPP, Map[Var, Val]) => Boolean): Boolean =
    for
      spp <- spps
      packet <- packets
    do
      if !f(spp, packet) then
        println(s"Failed for \n val spp = $spp \n val packet = $packet")
        return false
    true

  import scala.math.pow
  val spsSize = pow(2, vals.map { (v, s) => s.size + 1 }.product)
  val spssSize = pow(2, vals.map { (v, s) => s.size * s.size + s.size + 1 }.product)

import SPP._

val T = Tester(Map(0 -> Set(0, 1, 2), 1 -> Set(0, 1, 3), 2 -> Set(2, 3, 4)), random = true)
// val T = Tester(Map(0 -> Set(0, 1, 2)), random = true)
// val T = Tester(Map("x" -> Set(0, 1), "y" -> Set(0, 1), "z" -> Set(3, 5)), random = true)
// val T = Tester(Map("x" -> Set(0, 1), "y" -> Set(0)), random = true)
// val T = Tester(Map("x" -> Set(0), "y" -> Set(0)))

val x = "x"
val y = "y"
val z = "z"

T.sps.size
T.spsSize
T.spps.size
T.spssSize
T.packets.size

////////////////////// SP //////////////////////

// Check that SPs are correctly canonicalized
T.check_SP_SP { (sp1, sp2) =>
  if T.packets.forall { packet => SP.elemOf(packet, sp1) == SP.elemOf(packet, sp2) }
  then sp1 eq sp2
  else true
}

// Check SP.union
T.check_SP_SP_P { (sp1, sp2, packet) =>
  SP.elemOf(packet, SP.union(sp1, sp2)) == (SP.elemOf(packet, sp1) || SP.elemOf(packet, sp2))
}

// Check SP.difference
T.check_SP_SP_P { (sp1, sp2, packet) =>
  SP.elemOf(packet, SP.difference(sp1, sp2)) == (SP.elemOf(packet, sp1) && !SP.elemOf(packet, sp2))
}

val sp1 = SP.Test(0, HashMap(0 -> SP.True), SP.False)
val sp2 = SP.Test(0, HashMap(1 -> SP.True), SP.False)
val packet = HashMap(0 -> 0, 1 -> -1)
SP.difference(sp1, sp2)

// Check SP.intersection
T.check_SP_SP_P { (sp1, sp2, packet) =>
  SP.elemOf(packet, SP.intersection(sp1, sp2)) == (SP.elemOf(packet, sp1) && SP.elemOf(packet, sp2))
}

// Check SP.negate
T.check_SP_P { (sp, packet) =>
  SP.elemOf(packet, SP.negate(sp)) == !SP.elemOf(packet, sp)
}

////////////////////// SPP //////////////////////

// Check that SPPs are correctly canonicalized
T.check_SPP_SPP { (spp1, spp2) =>
  // Check that two spps are eq if they have the same run1 action on packets
  if T.packets.forall { packet => SPP.run1(packet, spp1) == SPP.run1(packet, spp2) }
  then spp1 eq spp2
  else true
}

T.check_SPP_nocanon { spp =>
  spp match {
    case SPP.False => true
    case SPP.Diag => true
    case SPP.TestMut(x, branches, other, id) =>
      val spp2 = SPP.TestMut(x, branches, other, id)
      T.packets.forall { packet => SPP.run1(packet, spp) == SPP.run1(packet, spp2) }
  }
}

// Check SPP.run
T.check_SPP_SP { (spp, sp) =>
  val sp2 = SPP.run(sp, spp)
  val results1 = T.packets.filter { p => SP.elemOf(p, sp) }.flatMap { p => SPP.run1(p, spp) }
  val results2 = T.packets.filter { p => SP.elemOf(p, sp2) }
  results1.toSet == results2.toSet
}

// Check SPP.toSPbackward
T.check_SPP_P { (spp, packet) =>
  val sp = SPP.toSPbackward(spp)
  if SP.elemOf(packet, sp) then !SPP.run1(packet, spp).isEmpty
  else SPP.run1(packet, spp).isEmpty
}

// Check SPP.fromSP
T.check_SP_P { (sp, p) =>
  val spp = SPP.fromSP(sp)
  SP.elemOf(p, sp) == !SPP.run1(p, spp).isEmpty
}

// Check SPP.seq with SPP.fromSP
T.check_SPP_SP_P { (spp, sp, p) =>
  val spp2 = SPP.seq(spp, SPP.fromSP(sp))
  SPP.run1(p, spp2) == SPP.run1(p, spp).filter { p => SP.elemOf(p, sp) }
}

// Check SPP.pull
T.check_SPP_SP { (spp, sp) =>
  val sp2 = SPP.pull(spp, sp)
  val results1 = T.packets.filter { p => SPP.run1(p, spp).exists(q => SP.elemOf(q, sp)) }
  val results2 = T.packets.filter { p => SP.elemOf(p, sp2) }
  results1.toSet == results2.toSet
}

// Check SPP.pull test 2
T.check_SPP_SP_P { (spp, sp, p) =>
  val sp2 = SPP.pull(spp, sp)
  if SP.elemOf(p, sp2) then SPP.run1(p, spp).exists(q => SP.elemOf(q, sp))
  else SPP.run1(p, spp).forall(q => !SP.elemOf(q, sp))
}

// Check SPP.seqSP
T.check_SPP_SP_P { (spp, sp, p) =>
  val results1 = SPP.run1(p, SPP.seqSP(sp, spp))
  val results2 = if SP.elemOf(p, sp) then SPP.run1(p, spp) else Set.empty
  results1 == results2
}

// Check SPP.union
T.check_SPP_SPP_P { (spp1, spp2, packet) =>
  SPP.run1(packet, SPP.union(spp1, spp2)) == SPP.run1(packet, spp1) ++ SPP.run1(packet, spp2)
}

// Check SPP.intersect
T.check_SPP_SPP_P { (spp1, spp2, packet) =>
  SPP.run1(packet, SPP.intersection(spp1, spp2)) == (SPP.run1(packet, spp1) & SPP.run1(packet, spp2))
}

// Check SPP.difference
T.check_SPP_SPP_P { (spp1, spp2, packet) =>
  SPP.run1(packet, SPP.difference(spp1, spp2)) == (SPP.run1(packet, spp1) -- SPP.run1(packet, spp2))
}

// Check SPP.seq
T.check_SPP_SPP_P { (spp1, spp2, packet) =>
  SPP.run1(packet, SPP.seq(spp1, spp2)) == SPP.run1(packet, spp1).flatMap { p => SPP.run1(p, spp2) }
}

// Check that SPP.run is the same as SPP.push
T.check_SPP_SP { (spp, sp) =>
  val sp2 = SPP.run(sp, spp)
  val sp3 = SPP.push(spp, sp)
  sp2 eq sp3
}
