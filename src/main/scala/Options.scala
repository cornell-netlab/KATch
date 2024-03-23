package nkpl

object Options {
  var warmup = false
  var supressOutput = false
  var convertToKat = false
  var inputFile = ""
  def katIndex() = s"kat/${inputFile.replace('/', '_')}_index.txt"
  def outputCSV = if warmup then "results/hotcomparison.csv" else "results/comparison.csv"
  var freneticTimeout = "86400s" // 24h
}
