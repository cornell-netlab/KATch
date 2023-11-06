import nkpl._

val x = "x"
val y = "y"
val z = "z"
val a = SP.Test(x, Map(1 -> SP.True, 2 -> SP.False), SP.False)
val b = SP.Test(y, Map(1 -> SP.False, 2 -> SP.True), SP.False)

SP.union(a, b)
