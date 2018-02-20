import java.util.UUID

class CandyDatabase(candies: Map[String, BigDecimal]) {
  val barcodes : Map[String,UUID] = candies.keySet.map((a) => 
  	(a, UUID.randomUUID())).toMap
  val prices : Map[UUID,BigDecimal] = barcodes.map({
  	case (k,v) => (v, candies.getOrElse(k, BigDecimal(0)))
  	})
  def getPriceFromBarcode(b: UUID) : Option[BigDecimal] = prices.get(b)
  def getBarcodeForCandy(c: String) : Option[UUID] = barcodes.get(c)
}