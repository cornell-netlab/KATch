
# NetKAT equivalence checker

Data structures:
- NK: NetKAT AST
- SP: symbolic packets (represents a set of packets)
- SPP: symbolic packet pairs (represents a nondeterministic transition)
- SMap: represents the δ part of a NetKAT expression in deterministic form (spp1⋅δ⋅e1 + spp2⋅δ⋅e2 + ... where the spp's are disjoint)

Operations:
- ε : NK → SPP
- δ : NK → SMap
- bisim : SPP × SPP → bool

## Potential improvements

- More tests
- Intersection, difference, and XOR operations in the NetKAT AST
- Lambdas in the checker language
- Partial dups
- Better error messages
- Better pretty printing and graphviz output
- Optimizations (the current implementation is very naive)
- Product automaton construction
- Documentation
- Reimplement in Rust