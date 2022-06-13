import org.scalactic.Accumulation._
import org.scalactic.{Bad, Every, Good, One, Or}

import scala.language.implicitConversions

sealed trait PaymentStatus {
  def toCellValue: String
}
object PaymentStatus {

  val PAYMENT_FOREGONE = "Payment Foregone"
  val PAYMENT_COMPLETED = "Payment Completed"
  val PAYMENT_NOT_REQUIRED = "Payment Not Required"

  def fromCellValue(value: String): Option[PaymentStatus] = value match {
    case PAYMENT_FOREGONE => Some(PaymentForegone)
    case PAYMENT_COMPLETED => Some(PaymentCompleted)
    case PAYMENT_NOT_REQUIRED => Some(PaymentNotRequired)
    case _ => None
  }
}
object PaymentForegone extends PaymentStatus {
  def toCellValue: String = PaymentStatus.PAYMENT_FOREGONE
}
object PaymentCompleted extends PaymentStatus {
  def toCellValue: String = PaymentStatus.PAYMENT_COMPLETED
}
object PaymentNotRequired extends PaymentStatus {
  def toCellValue: String = PaymentStatus.PAYMENT_NOT_REQUIRED
}

object Extensions {

  implicit class PaymentTableCell(val cell: Cell) {
    def asPaymentStatus: PaymentStatus Or One[InvalidCellTypeError] =
      cell.as(Cell.castAsOpt(cell.rowIndex, cell.colName)(PaymentStatus.fromCellValue))
  }

  implicit class PaymentTableRow(val row: Row) {
    def cellAsPaymentStatus(colName: String): PaymentStatus Or Every[CellReadError] = for {
      cell <- row.cell(colName)
      status <- cell.asPaymentStatus
    } yield status
  }
}

object Main extends App {

  import Extensions._

  val csvFilePath = "/home/harry/Desktop/metropolis_visit_data_iso8601.csv"
  val table = CSVTable(csvFilePath)

  val PAYMENT_STATUS = "payment_status"
  val ENTRY_TIME = "entry_time"
  val SITE_ID = "site_id"
  val PRICE = "price"

  def computeTotalRevenueByWeek(payments: Seq[Row],
                                entryTimeCol: String,
                                priceCol: String,
                                parseTimeCorrectly: Boolean): Iterable[(Int, Double)] Or Every[CellReadError] =
    payments
      .groupBy(payment => {
        if (parseTimeCorrectly) payment.cellAsDateTime(entryTimeCol).map(_.weekOfWeekyear().get())
        else payment.cellAsInt(entryTimeCol) // Intentionally parse date time using wrong type
      })
      .map {
        case (weekOrError, paymentsInWeek) => weekOrError match {
          case Good(week) => for {
            paymentPrices <- paymentsInWeek.map(_.cellAsDouble(priceCol)).combined
          } yield (week, paymentPrices.sum)
          case Bad(error) => Bad(error)
        }
      }.combined

  def computeWeeklyRevenueBySite(statusCol: String,
                                 entryTimeCol: String,
                                 siteIdCol: String,
                                 priceCol: String,
                                 parseTimeCorrectly: Boolean): Or[Seq[(String, Seq[(Int, Double)])], Every[CellReadError]] =
    for {
      payments <- table.select(statusCol, entryTimeCol, siteIdCol, PRICE)
      completedPayments = payments.filter(_.cellAsPaymentStatus(statusCol).exists(_.equals(PaymentCompleted)))
      completedPaymentsBySite <- completedPayments.groupBy(_.cellValue(SITE_ID)).map {
        case (siteIdOrError, sitePayments) => siteIdOrError match {
          case Good(siteId) => Good(siteId -> sitePayments)
          case Bad(error) => Bad(error)
        }
      }.combined
      weeklySiteRevenues <- completedPaymentsBySite.map {
        case (siteId, completedPayments) => for {
          totalRevenueByWeek <- computeTotalRevenueByWeek(completedPayments, entryTimeCol, priceCol, parseTimeCorrectly)
        } yield (siteId, totalRevenueByWeek.toSeq.sortBy{ case (week, _) => week })
      }.combined
    } yield weeklySiteRevenues.toSeq.sortBy{ case (siteId, _) => siteId }

  def printResult(weeklyRevenueBySiteOrError: Or[Seq[(String, Seq[(Int, Double)])], Every[CellReadError]]): Unit =
    weeklyRevenueBySiteOrError match {
      case Good(weeklyRevenueBySite) =>
        weeklyRevenueBySite.foreach {
          case (siteId, siteWeeklyRevenueMap) =>
            println(s"Site Id: $siteId")
            siteWeeklyRevenueMap.foreach {
              case (week, weeklyRevenue) =>
                println(s"Week: $week: $weeklyRevenue")
            }
            println()
        }

      // Some of the accumulated errors are duplicates
      // so we need to make sure to print only the unique ones
      case Bad(aggregateError) => aggregateError.toSeq.distinct.foreach(println(_))
    }

  // Good example where all the columns exist and data types are all valid
  val goodResult = computeWeeklyRevenueBySite(PAYMENT_STATUS, ENTRY_TIME, SITE_ID, PRICE, parseTimeCorrectly = true)
  println("Good result:")
  printResult(goodResult)

  // Bad example where one column does not exist
  val invalidEntryCol = "Invalid Entry Time Column"
  val invalidSiteIdCol = "SiteId"
  val missingColumnResult = computeWeeklyRevenueBySite(PAYMENT_STATUS, invalidEntryCol, invalidSiteIdCol, PRICE, parseTimeCorrectly = true)
  println("\n\nBad example where one or more columns do not exist:")
  printResult(missingColumnResult)

  // Bad example where entry time is cast using wrong data type
  val wrongDataTypeResult = computeWeeklyRevenueBySite(PAYMENT_STATUS, ENTRY_TIME, SITE_ID, PRICE, parseTimeCorrectly = false)
  println("\n\nBad example where entry time is cast using wrong data type:")
  printResult(wrongDataTypeResult)
}
