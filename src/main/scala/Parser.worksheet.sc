import nkpl._

val input = """x = (@dst←3 ∧ @pt←0 ⋅ @pt←0 ∪ @dst←4)? ⋆""" // your input string here
// val input = "x = (@dst←3)"
val result = Parser.parseStmt(input)
val n = input.length
