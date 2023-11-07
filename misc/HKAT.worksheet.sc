// Hash based NetKAT bismilarity checker
// ==========================

// Packet maps
// ===========
// The main data structure is a packet map, which maps packets to results.
// A packet is a vector of integers.
// A symbolic packet is a vector of integers, where some of the integers are -1.
// A symbolic packet represents a set of packets, where the -1s are wildcards.
// We want packet maps to be able to efficiently map packets to results.
// We therefore represent the packet map as a series of hash tables, where each hash table has a mask packet.
// In order to look up a packet in the packet map, we look up the packet in each of the hash
// tables, while applying the mask packet to the packet to set the wildcard positions to -1.
// The results of the different hash tables are then combined using an appropriate join operation.

// Deterministic packet maps
// ===================
// We can determinize a packet map, making it so that we only have to look up the packet in the most specific hash table.

// Lookup of symbolic packets
// ===================
// Lookup up a symbolic packet in a packet map is more complicated.
// A simple approach is to iterate over all packets in the packet map, and check if the packet
// matches the symbolic packet.
// For the initial version, we will use this approach.
// A more advanced approach would be to apply the mask associated with the symbolic packet to
// each of the hash tables, and cache the resulting hash tables.
// However, this approach is more complicated, and it is not clear if it will be more efficient for the benchmarks.

// Converting NetKAT expressions to packet maps
// ==============================
// We can represent a NetKAT expression as a packet map, that maps incoming packets to mutations
// to be applied to the packet, and the next NetKAT expression to be applied to the packet after the dup.
// The next NetKAT expression also has a extra boolean indicating whether this packet will be outputted.
// To build a NetKAT automaton, the primary operation is to convert a NetKAT expression to such a packet map.

// Benchmark instances
// ===================
// In each of the benchmarks, we have routing, which reads a packet's current location and destination, and updates the packet's outgoing port.
// We then have a topology, which reads a packet's current location and outgoing port, and updates the packet's location (and port, but that seems to be irrelevant).
// Combined, we read a packet's current location and destination, and update the packet's location (and port).
