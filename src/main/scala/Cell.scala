import org.joda.time.DateTime
import org.scalactic.{Bad, Good, One, Or}

import java.time.Instant
import scala.reflect.runtime.universe.TypeTag

case class Cell(rowIndex: Int, colName: String, cellValue: String) {

  def as[T](convert: String => T Or One[InvalidCellTypeError]): T Or One[InvalidCellTypeError] = convert(cellValue)

  def asOpt[T](implicit typeTag: TypeTag[T]): (String => Option[T]) => String => T Or One[InvalidCellTypeError] = Cell.castAsOpt[T](rowIndex, colName)

  def asInt(implicit typeTag: TypeTag[Int]): Int Or One[InvalidCellTypeError] = as(asOpt(typeTag)(_.toIntOption))

  def asLong(implicit typeTag: TypeTag[Long]): Long Or One[InvalidCellTypeError] = as(asOpt(typeTag)(_.toLongOption))

  def asFloat(implicit typeTag: TypeTag[Float]): Float Or One[InvalidCellTypeError] = as(asOpt(typeTag)(_.toFloatOption))

  def asDouble(implicit typeTag: TypeTag[Double]): Double Or One[InvalidCellTypeError] = as(asOpt(typeTag)(_.toDoubleOption))

  def asBool(implicit typeTag: TypeTag[Boolean]): Boolean Or One[InvalidCellTypeError] = as(asOpt(typeTag)(_.toBooleanOption))

  def asDateTime(implicit typeTag: TypeTag[DateTime]): DateTime Or One[InvalidCellTypeError] =
    as(value => try {
      Good(DateTime.parse(value))
    } catch {
      case _: Exception => Bad(One(InvalidCellTypeError(rowIndex, colName, typeTag.tpe.toString)))
    })

  def epochMilliCellAsInstant: Instant Or One[InvalidCellTypeError] = for {
    epochMilli <- asLong
  } yield Instant.ofEpochMilli(epochMilli)
}

object Cell {
  // For primitive types such as numeric types and boolean, conversion from string results in an Option[T].
  // If the Opt is non-empty, the conversion is successful and vice versa.
  // Since this logic is common for many primitive types, we'll use this curried function
  // to factor out this common logic to maximize code re-use.
  def castAsOpt[T](rowIndex: Int, colName: String)
                  (castAsOpt: String => Option[T])
                  (value: String)
                  (implicit typeTag: TypeTag[T]): T Or One[InvalidCellTypeError] =
    castAsOpt(value) match {
      case Some(result) => Good(result)
      case None => Bad(One(InvalidCellTypeError(rowIndex, colName, typeTag.tpe.toString)))
    }
}
