package nkpl

/** The `Options` object contains various configuration options for the program.
  */
object Options {

  /** Indicates whether JIT warmup is enabled (used for benchmarking).
    */
  var warmup = false

  /** Indicates whether output should be suppressed.
    */
  var suppressOutput = false

  /** Indicates whether the input should be converted to Frenetic's KAT format.
    */
  var convertToKat = false

  /** The path of the input file.
    */
  var inputFile = ""

  /** Returns the KAT index file path based on the input file.
    *
    * @return
    *   The KAT index file path.
    */
  def katIndex() = s"kat/${inputFile.replace('/', '_')}_index.txt"

  /** Returns the output CSV file path. If warmup is enabled, the output file is named `hotcomparison.csv`.
    *
    * @return
    *   The output CSV file path.
    */
  def outputCSV = if (warmup) "results/hotcomparison.csv" else "results/comparison.csv"

  /** The timeout value for Frenetic.
    */
  var freneticTimeout = "86400s" // 24h
}
