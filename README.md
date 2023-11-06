
# NetKAT bisimilarity checker

Data structures:
- NK: NetKAT AST
- SPk: Symbolic packets: represents a set of packets
- SPk[T]: represents a map Pk → T
- FDD: represents a transition, i.e. a function from a packet to a set of packets, i.e. a subset of (Pk, Pk)
- FDD[T]: represents a map (Pk, Pk) → T

- FDD[T] = SPk[Pk => T]


Operations:
- ε : NK → FDD
- δ : NK → FDD[NK]

- run : (SPK, FDD) → SPK  (run a transition on a set of packets)
- run : (SPK, FDD[T]) → SPK[T]  (run a transition on a set of packets)

Bisimilarity:
- bisim : (NK, NK) → Boolean
- todo : List[(NK, SPk, NK)]
- done : (NK, NK) => SPk

while (a,pk,b) = todo.pop {
  pk' = pk - done(a,b)
  // check for equivalence of ε's
  if (run(pk', ε(a)) != run(pk', ε(b))) return false
  pk1s = run(pk', δ(a))  : SPk[NK]
  pk2s = run(pk', δ(b))  : SPk[NK]
  for((a',pk1) in pk1s, (b',pk2) in pk2s) {
    todo.push((a', pk1 ∩ pk2, b'))
  }
  for((a',pk1) in pk1s) {
    if pk2s(a') == ∅ {
      todo.push((a', pk1, 0))
    }
  }
  for((b',pk2) in pk2s) {
    if pk1s(b') == ∅ {
      todo.push((0, pk2, b'))
    }
  }
}

Represent FDD[NK] as (NK => FDD)

Important: We only need SPk and FDD, we can represent SPk[T] as T => SPk and FDD[T] as T => FDD.
This works well as long as there are not very many different values of T that are used. In our case, T = NK, so this is fine as long as the number of different states in the automaton is not too large.

Data structures:
- SPk: ∩, -, =∅
- FDD: run(pk, fdd), ∩, ...

## Product automaton

S = (NK, NK)
δ2 : S → FDD[S]
δ2(a,b) = {(a',b') ↦ fdda ∩ fddb | (a',fdda) in δ(a), (b', fddb) in δ(b)}

ε2 : S → FDD
ε2(a,b) = ε(a) xor ε(b)

todo : List[(S, SPk)]
done : S => SPk

while (s,pk) = todo.pop {
  pk' = pk - done(s)
  done(s) ∪= pk'
  // check for equivalence of ε's
  if run(pk', ε2(s)) != ∅ return false
  pk'' = run(pk', δ2(s))  : SPk[S]
  for (s',pk''') in pk'' {
    todo.push((s',pk'''))
  }
}









## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).
