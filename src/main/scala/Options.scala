package nkpl

object Options {
  var warmup = false
  var supressOutput = false
  var convertToKat = false
  var inputFile = ""
  def katIndex() = s"kat/${inputFile.replace('/', '_')}_index.txt"
  var outputCSV = "results/comparison.csv"
  var freneticTimeout = "86400s" // 24h
}
