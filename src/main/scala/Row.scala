import org.joda.time.DateTime
import org.scalactic.{Bad, Every, Good, One, Or}

import java.time.Instant
import scala.reflect.runtime.universe.TypeTag

case class Row(index: Int, private val rawData: Map[String, String]) {

  private val cells: Map[String, Cell] = rawData.map {
    case (colName, cellValue) => (colName, Cell(index, colName, cellValue))
  }

  override def toString: String = s"Row $index:" + cells.map {
    case (key, cell) => s"$key -> ${cell.cellValue}"
  }.mkString("; ")

  def cell(colName: String): Cell Or One[ColumnNotFoundError] =
    cells.get(colName) match {
      case Some(cell) => Good(cell)
      case None => Bad(One(ColumnNotFoundError(colName)))
    }

  def cellAs[T](colName: String, cellValueToType: String => T Or One[InvalidCellTypeError]): T Or Every[CellReadError] = for {
    cell <- cell(colName)
    valueAsType <- cell.as(cellValueToType)
  } yield valueAsType

  def cellFromOptConversion[T](colName: String, cellValueToOpt: String => Option[T])
                              (implicit typeTag: TypeTag[T]): T Or Every[CellReadError] = for {
    cell <- cell(colName)
    valueAsType <- cell.asOpt[T](typeTag)(cellValueToOpt)(cell.cellValue)
  } yield valueAsType

  def cellValue(colName: String): String Or Every[CellReadError] = for {
    cell <- cell(colName)
  } yield cell.cellValue

  def cellAsInt(colName: String): Int Or Every[CellReadError] = cellFromOptConversion(colName, _.toIntOption)

  def cellAsLong(colName: String): Long Or Every[CellReadError] = cellFromOptConversion(colName, _.toLongOption)

  def cellAsFloat(colName: String): Float Or Every[CellReadError] = cellFromOptConversion(colName, _.toFloatOption)

  def cellAsDouble(colName: String): Double Or Every[CellReadError] = cellFromOptConversion(colName,_.toDoubleOption)

  def cellAsBool(colName: String): Boolean Or Every[CellReadError] = cellFromOptConversion(colName,_.toBooleanOption)

  def cellAsDateTime(colName: String)(implicit typeTag: TypeTag[DateTime]): DateTime Or Every[CellReadError] =
    cellAs(colName, value => try {
      Good(DateTime.parse(value))
    } catch {
      case _: Exception => Bad(One(InvalidCellTypeError(index, colName, typeTag.tpe.toString)))
    })

  def epochMilliCellAsInstant(colName: String): Instant Or Every[CellReadError] = for {
    epochMilli <- cellAsLong(colName)
  } yield Instant.ofEpochMilli(epochMilli)
}
