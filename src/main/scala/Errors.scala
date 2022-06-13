sealed abstract class CellReadError(val error: String) {
  override def toString: String = s"${this.getClass.getName}: $error"
}

case class ColumnNotFoundError(colName: String) extends CellReadError(s"Column '$colName' does not exist")

case class InvalidCellTypeError(rowIndex: Int, colName: String, typeName: String)
  extends CellReadError(s"Cell at row $rowIndex and column '$colName' is not of type '$typeName'")