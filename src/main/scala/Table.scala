import org.scalactic.Accumulation._
import org.scalactic.{Bad, Every, Good, One, Or}

trait Table {

  protected val rawData: Seq[Map[String, String]]

  def select(colNames: String*): Seq[Row] Or Every[ColumnNotFoundError] =
    rawData.zipWithIndex.map {
      case (rowData, index) => for {
        selectedData <- colNames.map(colName => rowData.get(colName) match {
          case Some(value) => Good(colName -> value)
          case None => Bad(One(ColumnNotFoundError(colName)))
        }).combined
      } yield Row(index, selectedData.toMap)
    }.combined

  override def toString: String = rawData.zipWithIndex.map {
    case (data, index) => Row(index, data).toString
  }.mkString("\n")
}
