case class CSVTable(csvFilePath: String) extends Table {

  protected val rawData: Seq[Map[String, String]] = CSVFileReader.readRawRowsWithHeaders(csvFilePath)
}
