import nkpl._

class Tester(vals: Map[Var, Set[Val]], random: Boolean = false):
  val sortedVals = vals.toList.sortBy { (v, s) => v }
  val packets =
    if false then (1 to 100).map { _ => randPacket() }.toSet
    else genComb(vals.map { (v, s) => v -> (s ++ Set(-1)) }).toList
  val sps =
    if random then (1 to 100).map { _ => randSP(sortedVals) }.toList.sortBy(sizeSP)
    else genSP(sortedVals).toList.sortBy(sizeSP)
  val spps =
    if random then (1 to 100).map { _ => randSPP(sortedVals) }.toList.sortBy(sizeSPP)
    else genSPP(sortedVals).toList.sortBy(sizeSPP)

  def randPacket(): Map[Var, Val] =
    vals.map { (v, s) => v -> s.toList(util.Random.nextInt(s.size)) }

  def randSubset[A](xs: Set[A]): Set[A] =
    xs.filter { _ => util.Random.nextBoolean() }

  def randSP(vals: List[(Var, Set[Val])]): SP =
    if vals.isEmpty then if util.Random.nextBoolean() then SP.False else SP.True
    else
      val (x, vs) = vals.head
      val rest = vals.tail
      val branches = randSubset(vs).map { v => v -> randSP(rest) }.toMap
      val other = randSP(rest)
      SP.Test(x, branches, other)

  def randSPP(vals: List[(Var, Set[Val])]): SPP =
    if vals.isEmpty then if util.Random.nextBoolean() then SPP.False else SPP.Diag
    else
      val (x, vs) = vals.head
      val rest = vals.tail
      val branches = randSubset(vs).map { v =>
        v -> randSubset(vs).map { v => v -> randSPP(rest) }.toMap
      }.toMap
      val other = randSubset(vs).map { v => v -> randSPP(rest) }.toMap
      val id = randSPP(rest)
      SPP.TestMut(x, branches, other, id)

  def genComb[A, B](xs: Map[A, Set[B]]): Set[Map[A, B]] =
    if xs.isEmpty then return Set(Map.empty)
    val (x, ys) = xs.head
    val rest = xs.tail
    val combs = genComb(rest)
    for
      comb <- combs
      y <- ys
    yield comb + (x -> y)

  def subMaps[A, B](xs: Map[A, B]): Set[Map[A, B]] =
    if (xs.isEmpty) Set(Map.empty[A, B])
    else
      val (x, y) = xs.head
      val rest = xs.tail
      val submaps = subMaps(rest)
      for {
        submap <- submaps
        result <- Set(submap, submap + (x -> y))
      } yield result

  def subMapss[A, B](xs: Set[Map[A, B]]): Set[Map[A, B]] =
    xs.flatMap(subMaps(_))

  // Generate all small SPs on a given set of variables and values
  def genSP(vars: List[(Var, Set[Val])]): Set[SP] =
    if vars.isEmpty then return Set(SP.False, SP.True)
    val (x, vals) = vars.head
    val sps = genSP(vars.tail)
    // We map each val to a SP. We also have a default SP
    for
      default <- sps
      ys <- subMapss(genComb(vals.map { v => v -> sps }.toMap))
    yield SP.Test(x, ys, default)

  def sizeSP(sp: SP): Int =
    sp match
      case SP.False => 1
      case SP.True => 1
      case SP.Test(x, branches, other) =>
        1 + branches.values.map(sizeSP).sum + sizeSP(other)

  def checkSP1(f: (Map[Var, Val], SP) => Boolean): Boolean =
    for
      sp <- sps
      packet <- packets
    do
      if !f(packet, sp) then
        println(s"Failed for $sp and $packet")
        return false
    true

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
    val others = subMapss(genComb(vals.map { v => v -> spps }.toMap))
    val branchess = subMapss(genComb(vals.map { v => v -> others }.toMap))
    for
      branches <- branchess
      other <- others
      id <- spps
    yield SPP.TestMut(x, branches, other, id)

  def sizeSPP(spp: SPP): Int =
    spp match
      case SPP.False => 1
      case SPP.Diag => 2
      case SPP.TestMut(x, branches, other, id) =>
        1 + branches.values.map { muts => muts.values.map(sizeSPP).sum + 1 }.sum + other.values.map(sizeSPP).sum + sizeSPP(id)

  def check_SPP_SP(f: (SPP, SP) => Boolean): Boolean =
    for
      spp <- spps
      sp <- sps
    do
      if !f(spp, sp) then
        println(s"Failed for \n val sp = $sp \n val spp = $spp")
        return false
    true

  def check_SPP_SPP(f: (SPP, SPP) => Boolean): Boolean =
    for
      spp1 <- spps
      spp2 <- spps
    do
      if !f(spp1, spp2) then
        println(s"Failed for \n val spp1 = $spp1 \n val spp2 = $spp2")
        return false
    true

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

  def check_SP_SP(f: (SP, SP) => Boolean): Boolean =
    for
      sp1 <- sps
      sp2 <- sps
    do
      if !f(sp1, sp2) then
        println(s"Failed for \n val sp1 = $sp1 \n val sp2 = $sp2")
        return false
    true

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

  def check_SP_P(f: (SP, Map[Var, Val]) => Boolean): Boolean =
    for
      sp <- sps
      packet <- packets
    do
      if !f(sp, packet) then
        println(s"Failed for \n val sp = $sp \n val packet = $packet")
        return false
    true

  import scala.math.pow
  val spsSize = pow(2, vals.map { (v, s) => s.size + 1 }.product)
  val spssSize = pow(2, vals.map { (v, s) => s.size * s.size + s.size + 1 }.product)

import SPP._

val T = Tester(Map("x" -> Set(0, 1), "y" -> Set(0, 1), "z" -> Set(3)), random = true)
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

// Check SP.intersection
T.check_SP_SP_P { (sp1, sp2, packet) =>
  SP.elemOf(packet, SP.intersection(sp1, sp2)) == (SP.elemOf(packet, sp1) && SP.elemOf(packet, sp2))
}

// Check SP.negate
T.check_SP_P { (sp, packet) =>
  SP.elemOf(packet, SP.negate(sp)) == !SP.elemOf(packet, sp)
}

////////////////////// SPP //////////////////////

// // Check that SPPs are correctly canonicalized
// T.check_SPP_SPP { (spp1, spp2) =>
//   // Check that two spps are eq if they have the same run1 action on packets
//   if T.packets.forall { packet => SPP.run1(packet, spp1) == SPP.run1(packet, spp2) }
//   then spp1 eq spp2
//   else true
// }

// // Check SPP.run
// T.check_SPP_SP { (spp, sp) =>
//   val sp2 = SPP.run(sp, spp)
//   val results1 = T.packets.filter { p => SP.elemOf(p, sp) }.flatMap { p => SPP.run1(p, spp) }
//   val results2 = T.packets.filter { p => SP.elemOf(p, sp2) }
//   results1.toSet == results2.toSet
// }

// // Check SPP.seqSP
// T.check_SPP_SP_P { (spp, sp, p) =>
//   val results1 = SPP.run1(p, SPP.seqSP(sp, spp))
//   val results2 = if SP.elemOf(p, sp) then SPP.run1(p, spp) else Set.empty
//   results1 == results2
// }

// // Check SPP.union
// T.check_SPP_SPP_P { (spp1, spp2, packet) =>
//   SPP.run1(packet, SPP.union(spp1, spp2)) == SPP.run1(packet, spp1) ++ SPP.run1(packet, spp2)
// }

// // Check SPP.intersect
// T.check_SPP_SPP_P { (spp1, spp2, packet) =>
//   SPP.run1(packet, SPP.intersection(spp1, spp2)) == (SPP.run1(packet, spp1) & SPP.run1(packet, spp2))
// }

// // Check SPP.difference
// T.check_SPP_SPP_P { (spp1, spp2, packet) =>
//   SPP.run1(packet, SPP.difference(spp1, spp2)) == (SPP.run1(packet, spp1) -- SPP.run1(packet, spp2))
// }

// // Check SPP.seq
// T.check_SPP_SPP_P { (spp1, spp2, packet) =>
//   SPP.run1(packet, SPP.seq(spp1, spp2)) == SPP.run1(packet, spp1).flatMap { p => SPP.run1(p, spp2) }
// }
