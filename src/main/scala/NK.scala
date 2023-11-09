package nkpl

type Var = String
type Val = Int

class NK
case object Dup extends NK {
  override def toString: String = "δ"
}
case class Test(x: Var, v: Val) extends NK {
  override def toString: String = s"@$x=$v"
}
case class TestNE(x: Var, v: Val) extends NK {
  override def toString: String = s"@$x≠$v"
}
case class Mut(x: Var, v: Val) extends NK {
  override def toString: String = s"@$x←$v"
}
case class Seq(es: List[NK]) extends NK {
  override def toString: String =
    if es.isEmpty then "ε"
    else "(" + es.mkString("⋅") + ")"
}
case class Sum(es: Set[NK]) extends NK {
  override def toString: String =
    if es.isEmpty then "∅"
    else "(" + es.mkString("∪") + ")"
}
case class Difference(e1: NK, e2: NK) extends NK {
  override def toString: String = s"(($e1)∖($e2))"
}
case class Intersection(e1: NK, e2: NK) extends NK {
  override def toString: String = s"(($e1)∩($e2))"
}
case class XOR(e1: NK, e2: NK) extends NK {
  override def toString: String = s"(($e1)⊕($e2))"
}
case class Star(e: NK) extends NK {
  override def toString: String = s"(${e})⋆"
}
case class VarName(x: String) extends NK

object Test {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Test, Test]
  def apply(x: Var, v: Val): NK =
    val w = new Test(x, v)
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

var clearCachesFns = List[() => Unit]()
def clearCaches(): Unit =
  clearCachesFns.foreach { f => f() }

def memoize[A, B](f: A => B): A => B =
  val cache = scala.collection.mutable.Map.empty[A, B]
  clearCachesFns = (() => cache.clear()) :: clearCachesFns
  (a: A) => cache.getOrElseUpdate(a, f(a))

// Memoizes functions that take more arguments.
def memoize2[A1, A2, B](f: (A1, A2) => B): (A1, A2) => B =
  val g = memoize(f.tupled)
  (a1, a2) => g((a1, a2))
def memoize3[A1, A2, A3, B](f: (A1, A2, A3) => B): (A1, A2, A3) => B =
  val g = memoize(f.tupled)
  (a1, a2, a3) => g((a1, a2, a3))
