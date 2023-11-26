# Symbolic verification for NetKAT

Most relevant previous work:
- Nate's NetKAT decision procedure paper (POPL'15, https://dl.acm.org/doi/10.1145/2676726.2677011) --> not (fully) symbolic + no counterexamples (?) --> huge speed difference
- Damien's Symbolic KAT paper (POPL'15, https://dl.acm.org/doi/10.1145/2676726.2677007) --> doesn't support NetKAT --> we need to explain why NetKAT is harder and requires new ideas

Contribution: Symbolic decision procedure for NetKAT
- More efficient and more scalable
- Symbolic counterexamples (both input and output)
- More operations (xor, intersection, difference --> can express more verification questions)
- Key ingredient: backward algorithm
- Key ingredient: SPP data structure, with mutations in the middle
- Key ingredient: NetKAT Brzozowski derivative
- Benchmark suite

Story for forward vs backward: we don't compare them for efficiency; they answer different questions when testing A ≡ B, beyond just saying "yes" or "no":
- Forward: which output packets come out differently from A and B? (i.e., which output packets can come out of A xor B)
- Backward: which input packets cause A and B to behave differently? (i.e., which input packets aren't dropped by A xor B)
We can still compare them for how efficiently they answer the yes/no question.
(similar story for A ⊆ B)

Further story: two regimes of NetKAT programs, and which part of the implementation they stress.
- Regime A: big network with simple behavior --> think: big database joins, datalog
- Regime B: small network with complex behavior -> think: SAT solvers, BDDs

Symbolic representation is important for both regimes, but:
- Regime A: need to have efficient joins
- Regime B: need hash consing and memoization to avoid exponential blowup

SPP data structure solves both problems:
- Efficient joins: SPPs are like tries, so we can do joins efficiently. Important: storing mutations in the middle allows us to do a triejoin *even for sequential composition* (which is the main cost)
- Hash consing and memoization: SPPs are like BDDs, so we can hash cons them. Important: storing mutations in the middle gives exponential speedup for programs that modify the packet locally.
  (for example, consider a packet with 20 bits, and a netkat program that flips all bits in the input packet --> linear representation with SPP, exponential with FDD)

Another key idea is the NetKAT Brzozowski derivative, which I think is also an important reason why the implementation is fast.
This basically lets you do the expensive work up front only once, instead of doing it during the bisimulation iteration.

In terms of technical contributions, both the SPP data structure, the backward algorithm, and to some extent the Brzozowski derivative are interesting and probably novel.
I think that each of these are technically nontrivial. Perhaps there are other technical contributions as well.

# Paper structure

- Introduction
- NetKAT background: syntax and semantics
  - How networks are modeled
  - Extended NetKAT
  - Forward and backward verification questions
- Symbolic representation
  - Tests / dup-free programs / full programs
  - SP/SPP representation
  - NetKAT automata, Brozowski derivative
- Forward and backward algorithms
- Implementation
- Evaluation
- Related work