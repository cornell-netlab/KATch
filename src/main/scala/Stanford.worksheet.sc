import scala.io.Source
import scala.util.Using

case class Edge(destination: String, sport: Int, dport: Int)

def parseGraphDefinition(graphDefinition: String): (List[String], Map[String, List[Edge]]) = {
  val nodePattern = """(\w+)\s+\[type=router\];""".r
  val edgePattern = """(\w+)\s+->\s+(\w+)\s+\[sport=(\d+),dport=(\d+)\];""".r

  val nodes = nodePattern.findAllMatchIn(graphDefinition).map(_.group(1)).toList.sorted
  val edges = edgePattern.findAllMatchIn(graphDefinition).foldLeft(Map.empty[String, List[Edge]]) { (acc, m) =>
    val source = m.group(1)
    val edge = Edge(m.group(2), m.group(3).toInt, m.group(4).toInt)
    acc + (source -> (edge :: acc.getOrElse(source, List.empty[Edge])))
  }

  (nodes, edges)
}

def readGraphFromFile(filePath: String): String = {
  Using(Source.fromFile(filePath)) { source =>
    source.mkString
  }.getOrElse(throw new Exception("Failed to read graph file."))
}

val filePath = "stanford/topology.dot"
val graphContent = readGraphFromFile(filePath)
val (nodes, edges) = parseGraphDefinition(graphContent)

val outFilePath = "nkpl/stanford/topology.nkpl"
val outFile = new java.io.PrintWriter(outFilePath)
outFile.println("-- Routers:")
for ((node, i) <- nodes.zipWithIndex) {
  outFile.println(s"$node = $i")
}
outFile.println("\n-- Routing:")
edges.foreach { case (source, edgeList) =>
  outFile.println(s"${source}_route = ${edgeList.map(edge => s"@port=${edge.sport} ⋅ @port←${edge.dport} ⋅ @sw←${edge.destination}").mkString(" + ")}")
}
outFile.println("\n-- Topology:")
outFile.println(s"topo = ${nodes.map(node => s"@sw=$node ⋅ ${node}_route").mkString(" + ")}")
outFile.close()

// OpenFlow rules

val rulesFilePath = "stanford/bbra_rtr.of"
val rulesContent = readGraphFromFile(rulesFilePath)

// The file contains json with entries of the form:
// {"in_ports": [100001, 100002, 100003, 100004, 100005, 100006, 100007, 100008, 100009, 100010, 100011], "ip_dst_wc": 12, "ip_dst_new": null, "out_ports": [100008], "ip_dst_match": 2873098344},

case class Rule(inPorts: List[Int], wildcard: Int, outPorts: List[Int], ip: Long)

// We parse using regex
val rulePattern = """\{"in_ports": \[([0-9, ]*)\], "ip_dst_wc": (\d+), "ip_dst_new": null, "out_ports": \[([0-9, ]*)\], "ip_dst_match": (\d+)\},""".r

def parseIntList(s: String): List[Int] = {
  if (s.isEmpty) {
    List.empty
  } else {
    s.split(", ").toList.map(_.toInt)
  }
}

val rules = rulePattern
  .findAllMatchIn(rulesContent)
  .map { m =>
    try {
      Rule(parseIntList(m.group(1)), m.group(2).toInt, parseIntList(m.group(3)), m.group(4).toLong)
    } catch {
      case e: Exception => throw new Exception(s"Failed to parse rule: ${m.matched}", e)
    }
  }
  .toList
