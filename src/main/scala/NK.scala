package nkpl

/** This file defines the types `Var` and `Val`.
  *
  *   - `Var` represents a variable and is of type `Int`.
  *   - `Val` represents a value and is of type `Int`.
  */
type Var = Int
type Val = Int

/** The base class for all NetKAT expressions.
  */
class NK

/** Represents the duplication operator (δ) in NetKAT.
  */
case object Dup extends NK {
  override def toString: String = "δ"
}

/** Represents a test expression (@x=v) in NetKAT.
  * @param x
  *   The variable name.
  * @param v
  *   The value to test against.
  */
case class Test(x: Var, v: Val) extends NK {
  override def toString: String = s"@$x=$v"
}

/** Represents a negated test expression (@x≠v) in NetKAT.
  * @param x
  *   The variable name.
  * @param v
  *   The value to test against.
  */
case class TestNE(x: Var, v: Val) extends NK {
  override def toString: String = s"@$x≠$v"
}

/** Represents a mutation expression (@x←v) in NetKAT.
  * @param x
  *   The variable name.
  * @param v
  *   The value to assign.
  */
case class Mut(x: Var, v: Val) extends NK {
  override def toString: String = s"@$x←$v"
}

/** Represents a sequence of NetKAT expressions (e1⋅e2⋅...⋅en) in NetKAT.
  * @param es
  *   The list of expressions in the sequence.
  */
case class Seq(es: List[NK]) extends NK {
  override def toString: String =
    if es.isEmpty then "ε"
    else "(" + es.mkString("⋅") + ")"
  override val hashCode = es.hashCode()
}

/** Represents a set of NetKAT expressions (e1∪e2∪...∪en) in NetKAT.
  * @param es
  *   The set of expressions.
  */
case class Sum(es: Set[NK]) extends NK {
  override def toString: String =
    if es.isEmpty then "∅"
    else "(" + es.mkString("∪") + ")"
  override val hashCode = es.hashCode()
}

/** Represents the difference of two NetKAT expressions (e1∖e2) in NetKAT.
  * @param e1
  *   The first expression.
  * @param e2
  *   The second expression.
  */
case class Difference(e1: NK, e2: NK) extends NK {
  override def toString: String = s"(($e1)∖($e2))"
}

/** Represents the intersection of two NetKAT expressions (e1∩e2) in NetKAT.
  * @param e1
  *   The first expression.
  * @param e2
  *   The second expression.
  */
case class Intersection(e1: NK, e2: NK) extends NK {
  override def toString: String = s"(($e1)∩($e2))"
}

/** Represents the exclusive OR of two NetKAT expressions (e1⊕e2) in NetKAT.
  * @param e1
  *   The first expression.
  * @param e2
  *   The second expression.
  */
case class XOR(e1: NK, e2: NK) extends NK {
  override def toString: String = s"(($e1)⊕($e2))"
}

/** Represents the Kleene star of a NetKAT expression (e⋆) in NetKAT.
  * @param e
  *   The expression.
  */
case class Star(e: NK) extends NK {
  override def toString: String = s"(${e})⋆"
}

/** Represents a test expression with a state predicate (test SP) in NetKAT.
  * @param e
  *   The state predicate.
  */
case class TestSP(e: SP) extends NK {
  override def toString: String = s"(test ${e})"
}

/** Represents a variable name in NetKAT.
  * @param x
  *   The variable name.
  */
case class VarName(x: String) extends NK

/* The following are for hash consing NetKAT expressions */

object Test {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Test, Test]
  def apply(x: Var, v: Val): NK =
    val w = new Test(x, v)
    cache.getOrElseUpdate(w, w)
}

object TestNE {
  private val cache = scala.collection.mutable.WeakHashMap.empty[TestNE, TestNE]
  def apply(x: Var, v: Val): NK =
    val w = new TestNE(x, v)
    cache.getOrElseUpdate(w, w)
}

object TestSP {
  private val cache = scala.collection.mutable.WeakHashMap.empty[TestSP, TestSP]
  def apply(e: SP): NK =
    val w = new TestSP(e)
    cache.getOrElseUpdate(w, w)
}

object Intersection {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Intersection, Intersection]
  def apply(e1: NK, e2: NK): NK =
    val w = new Intersection(e1, e2)
    cache.getOrElseUpdate(w, w)
}

object XOR {
  private val cache = scala.collection.mutable.WeakHashMap.empty[XOR, XOR]
  def apply(e1: NK, e2: NK): NK =
    val w = new XOR(e1, e2)
    cache.getOrElseUpdate(w, w)
}

object Difference {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Difference, Difference]
  def apply(e1: NK, e2: NK): NK =
    val w = new Difference(e1, e2)
    cache.getOrElseUpdate(w, w)
}

object Mut {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Mut, Mut]
  def apply(x: Var, v: Val): NK =
    val w = new Mut(x, v)
    cache.getOrElseUpdate(w, w)
}

object Seq {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Seq, Seq]
  def apply(es: List[NK]): NK =
    val es2 = es.flatMap { case Seq(es) => es; case e => List(e) }
    if es2.length == 1 then return es2.head
    if es2.contains(Zero) then return Zero
    val w = new Seq(es2)
    cache.getOrElseUpdate(w, w)
}

object Sum {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Sum, Sum]
  def apply(es: Set[NK]): NK =
    val es2 = es.flatMap { case Sum(es) => es; case e => Set(e) }
    if es2.size == 1 then return es2.head
    val w = new Sum(es2)
    cache.getOrElseUpdate(w, w)
}

object Star {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Star, Star]
  def apply(e: NK): NK =
    e match {
      case Sum(es) if es.isEmpty => Seq(List())
      case Seq(es) if es.isEmpty => Seq(List())
      case Star(_) => e
      case _ =>
        val w = new Star(e)
        cache.getOrElseUpdate(w, w)
    }
}

val Zero = Sum(Set())
val One = Seq(List())

// List of functions to clear the caches
var clearCachesFns = List[() => Unit]()

/** Clears the memoization caches by invoking the clear method on each cache.
  */
def clearCaches(): Unit =
  SP.Test.cache.clear()
  SPP.TestMut.cache.clear()
  clearCachesFns.foreach { f => f() }

/** Memoizes a function by caching its results.
  *
  * @param f
  *   The function to memoize.
  * @tparam A
  *   The type of the input argument.
  * @tparam B
  *   The type of the output value.
  * @return
  *   The memoized function.
  */
def memoize[A, B](f: A => B): A => B =
  val cache = scala.collection.mutable.HashMap.empty[A, B]
  clearCachesFns = (() => cache.clear()) :: clearCachesFns
  (a: A) => cache.getOrElseUpdate(a, f(a))

/** Memoizes a function that takes two arguments by caching its results.
  *
  * @param f
  *   The function to memoize.
  * @tparam A1
  *   The type of the first input argument.
  * @tparam A2
  *   The type of the second input argument.
  * @tparam B
  *   The type of the output value.
  * @return
  *   The memoized function.
  */
def memoize2[A1, A2, B](f: (A1, A2) => B): (A1, A2) => B =
  val g = memoize(f.tupled)
  (a1, a2) => g((a1, a2))

/** Memoizes a function that takes three arguments by caching its results.
  *
  * @param f
  *   The function to memoize.
  * @tparam A1
  *   The type of the first input argument.
  * @tparam A2
  *   The type of the second input argument.
  * @tparam A3
  *   The type of the third input argument.
  * @tparam B
  *   The type of the output value.
  * @return
  *   The memoized function.
  */
def memoize3[A1, A2, A3, B](f: (A1, A2, A3) => B): (A1, A2, A3) => B =
  val g = memoize(f.tupled)
  (a1, a2, a3) => g((a1, a2, a3))
