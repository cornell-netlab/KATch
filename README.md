
# NetKAT equivalence checker

Data structures:
- NK: NetKAT AST
- SP: symbolic packets (represents a set of packets)
- SPP: symbolic packet pairs (represents a nondeterministic transition)
- SMap: represents the δ part of a NetKAT expression in deterministic form (spp1⋅δ⋅e1 + spp2⋅δ⋅e2 + ... where the spp's are disjoint)

Primary operations:
- ε : NK → SPP
- δ : NK → SMap
- bisim : SPP × SPP → bool

## Potential improvements

- Graphical output
- Partial dups
- Better error messages
- More canonicalization
- Query optimizer
- Mark's "havoc" operator
- Minimal automata
- The Φ dup erasure operator
- Packet parsing / types
- Import / export routing tables
- Union find
- Grammars beyond regular expressions (Floyd grammars?)
- SMT
- Verified version in Lean