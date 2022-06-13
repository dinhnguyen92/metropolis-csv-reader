import org.scalatest._
import flatspec._
import matchers._
import org.scalactic.Accumulation._
import org.scalactic.{Bad, Many}

class TableSpec extends AnyFlatSpec with should.Matchers {

 case class MockTable(seeData: Seq[Map[String, String]]) extends Table {
   override protected val rawData: Seq[Map[String, String]] = seeData
 }

  val INT_COL = "IntCol"
  val BOOL_COL = "BoolCol"
  val STRING_COL = "StringCol"

  val mockTable: MockTable = MockTable(Seq(
    Map(INT_COL -> "1", BOOL_COL -> "false", STRING_COL -> "row0"),
    Map(INT_COL -> "5", BOOL_COL -> "true", STRING_COL -> "row1")
  ))

  it should "return ColumnNotFoundError when the specified column does not exist" in {
    val colName = "NonExistentColumn"
    val result = for {
      rows <- mockTable.select(colName)
    } yield rows
    result shouldEqual Bad(Many(ColumnNotFoundError(colName), ColumnNotFoundError(colName)))
  }

  it should "return InvalidDataTypeError when the specified cell type is invalid" in {
    val result = for {
      rows <- mockTable.select(STRING_COL)
      cells <- rows.map(_.cellAsInt(STRING_COL)).combined
    } yield cells
    result shouldEqual Bad(Many(InvalidCellTypeError(0, STRING_COL, "Int"), InvalidCellTypeError(1, STRING_COL, "Int")))
  }
}
