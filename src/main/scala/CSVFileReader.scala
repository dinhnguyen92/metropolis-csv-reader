import com.github.tototoshi.csv.CSVReader

import java.io.File
import scala.util.Using

// More details on how to use tototoshi csv reader here:
// https://github.com/tototoshi/scala-csv
object CSVFileReader {

  def readRawRowsWithHeaders(csvFilePath: String): Array[Map[String, String]] =
    Using.resource(CSVReader.open(new File(csvFilePath))) {
      _.allWithHeaders().map(normalizeRow).filterNot(rowIsEmpty).toArray
    }

  private def normalizeRow(row: Map[String, String]): Map[String, String] = row map {
    case (key, value) => key -> value.trim
  }

  private def rowIsEmpty(row: Map[String, String]): Boolean = row.forall {
    case (_, value) => value.isEmpty
  }
}
