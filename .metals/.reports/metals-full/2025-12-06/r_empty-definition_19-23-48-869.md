error id: file:///C:/02%20PROJECTS/assingment-2-lani/src/main/scala/myfirstscala.scala:scala/Option#
file:///C:/02%20PROJECTS/assingment-2-lani/src/main/scala/myfirstscala.scala
empty definition using pc, found symbol in pc: scala/Option#
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -Option#
	 -scala/Predef.Option#
offset: 1742
uri: file:///C:/02%20PROJECTS/assingment-2-lani/src/main/scala/myfirstscala.scala
text:
```scala
import scalafx.application.JFXApp3

import scala.io.Source
import scala.util.Try

// Immutable data class to store booking
case class Booking(
                    bookingId: String,
                    dateOfBooking: String,
                    time: String,
                    customerId: String,
                    gender: String,
                    age: Int,
                    originCountry: String,
                    state: String,
                    location: String,
                    destinationCountry: String,
                    destinationCity: String,
                    numPeople: Int,
                    checkInDate: String,
                    numDays: Int,
                    checkOutDate: String,
                    rooms: Int,
                    hotelName: String,
                    hotelRating: Double,
                    paymentMode: String,
                    bankName: String,
                    bookingPrice: Double,
                    discount: Double,
                    gst: Double,
                    profitMargin: Double
                  )

object MyApp extends JFXApp3:

  // Load CSV (load + parse + convert)
  def loadBookings(file: String): List[Booking] =
    val stream = getClass.getResourceAsStream(s"/$file")
    if stream == null then
      println(s"ERROR: Could not load $file")
      return Nil

    val lines = Source.fromInputStream(stream, "ISO-8859-1").getLines().toList // Use a permissive single-byte encoding to avoid charset errors from the CSV file

    lines
      .drop(1)             // remove header
      .flatMap(parseCSVLine)
      .flatMap(convertToBooking)

  // Parse CSV
  def parseCSVLine(line: String): Op@@tion[Array[String]] =
    val buffer = scala.collection.mutable.ArrayBuffer[String]()
    val sb = new StringBuilder
    var insideQuotes = false

    var i = 0
    while i < line.length do
      val c = line.charAt(i)

      // safer, handles quoted commas
      c match
        case '"' =>
          insideQuotes = !insideQuotes

        case ',' =>
          if insideQuotes then
            sb.append(c)
          else
            buffer.append(sb.toString.trim)
            sb.setLength(0)

        case _ =>
          sb.append(c)

      i += 1

    // Add last field
    buffer.append(sb.toString.trim)

    Some(buffer.toArray)

  // Convert CSV to Bookings data structure
  def convertToBooking(cols: Array[String]): Option[Booking] =
    if cols.length < 24 then
      println(s"Skipping row (invalid column count): ${cols.mkString("|")}")
      return None

    Try {
      Booking(
        cols(0), cols(1), cols(2), cols(3), cols(4),
        cols(5).toInt,
        cols(6), cols(7), cols(8),
        cols(9), cols(10),
        cols(11).toInt,
        cols(12), cols(13).toInt, cols(14),
        cols(15).toInt,
        cols(16), cols(17).toDouble, cols(18), cols(19),
        cols(20).toDouble,
        cols(21).replace("%", "").toDouble,
        cols(22).toDouble,
        cols(23).toDouble
      )
    }.toOption

  // Helper Function: Key to group hotels by (name, city, country)
  case class HotelKey(name: String, city: String, country: String)

  // Helper Function: Calculates totals for price, people discount, profit and bookings of each hotel group
  case class HotelAgg(totalPrice: Double, totalPeople: Int, totalDiscount: Double, totalProfit: Double, bookings: Int):
    def +(o: HotelAgg) = HotelAgg(totalPrice + o.totalPrice, totalPeople + o.totalPeople, totalDiscount + o.totalDiscount, totalProfit + o.totalProfit, bookings + o.bookings)

  // Helper Function: Get core calculations shared across questions
  class HotelAnalytics(data: List[Booking]):
    // Map HotelAgg to HotelKey (Find totals for each hotel group)
    private val aggs: Map[HotelKey, HotelAgg] =
      data
        .groupMapReduce(b => HotelKey(b.hotelName, b.destinationCity, b.destinationCountry))(
          b => HotelAgg(b.bookingPrice, b.numPeople, b.discount, b.profitMargin, 1)
        )(_ + _)

    // Find average price, disocunt, profit and total people for that hotel group
    private lazy val metrics: Map[HotelKey, (Double, Double, Double, Int)] =
      val groups: Map[HotelKey, List[Booking]] = data.groupMap(b => HotelKey(b.hotelName, b.destinationCity, b.destinationCountry))(b => b).view.mapValues(_.toList).toMap

      groups.view.mapValues { g =>
        // bookingPrice / numPeople (per booking, not calculated as group)
        val perBookingPrices = g.map { b => if b.numPeople > 0 then b.bookingPrice / b.numPeople else b.bookingPrice }
        
        // per group calculation
        val avgPricePerPerson = if perBookingPrices.isEmpty then 0.0 else perBookingPrices.sum / perBookingPrices.size
        val avgDiscount = if g.isEmpty then 0.0 else g.map(_.discount).sum / g.size
        val avgProfit = if g.isEmpty then 0.0 else g.map(_.profitMargin).sum / g.size
        val totalPeople = g.map(_.numPeople).sum
        (avgPricePerPerson, avgDiscount, avgProfit, totalPeople)
      }.toMap

    // Q1: countries with most bookings
    def topCountries(): (List[String], Int) =
      val counts = data.groupMapReduce(_.destinationCountry)(_ => 1)(_ + _)
      if counts.isEmpty then (Nil, 0)
      else
        val maxCount = counts.values.max
        val top = counts.collect { case (k, v) if v == maxCount => k }.toList.sorted
        (top, maxCount)

    // Detailed country counts (descending)
    def countryCounts(): List[(String, Int)] =
      // Counts by destination country
      data.groupMapReduce(_.destinationCountry)(_ => 1)(_ + _).toList.sortBy(-_._2)

    // Q2: most economical hotel using normalized (pricePerPerson low, discount high, profitMargin low)
    def mostEconomical(): Option[(String, Double, Double, Double, Double)] =
      if metrics.isEmpty then None
      else
        val priceVals = metrics.values.map(_._1).toList
        val discVals  = metrics.values.map(_._2).toList
        val profVals  = metrics.values.map(_._3).toList

        val minP = priceVals.min; val maxP = priceVals.max
        val minD = discVals.min;  val maxD = discVals.max
        val minR = profVals.min;  val maxR = profVals.max

        def norm(v: Double, lo: Double, hi: Double): Double = if hi == lo then 1.0 else (v - lo) / (hi - lo)

        val scored = metrics.iterator.map { case (k, (p, d, r, people)) =>
          val priceScore = 1.0 - norm(p, minP, maxP)
          val discScore  = norm(d, minD, maxD)
          val profScore  = 1.0 - norm(r, minR, maxR)
          val score = (priceScore + discScore + profScore) / 3.0
          (k, p, d, r, score)
        }.toList

        val best = scored.maxBy(_._5)
        val (k, p, d, r, s) = best
        Some((s"${k.name}, ${k.city}, ${k.country}", p, d, r, s))

    // Full economical rankings (desc by score)
    def economicalRankings(): List[(String, Double, Double, Double, Double)] =
      val priceVals = metrics.values.map(_._1).toList
      val discVals  = metrics.values.map(_._2).toList
      val profVals  = metrics.values.map(_._3).toList
      val minP = priceVals.min; val maxP = priceVals.max
      val minD = discVals.min;  val maxD = discVals.max
      val minR = profVals.min;  val maxR = profVals.max
      def norm(v: Double, lo: Double, hi: Double): Double = if hi == lo then 1.0 else (v - lo) / (hi - lo)
      metrics.iterator.map { case (k, (p, d, r, people)) =>
        val priceScore = 1.0 - norm(p, minP, maxP)
        val discScore  = norm(d, minD, maxD)
        val profScore  = 1.0 - norm(r, minR, maxR)
        val score = (priceScore + discScore + profScore) / 3.0
        (s"${k.name}, ${k.city}, ${k.country}", p, d, r, score)
      }.toList.sortBy(-_._5)

    // Q3: lecturer-style score using visitor totals + avg profit margin (normalized, averaged)
    def mostProfitable(): Option[(String, Double)] =
      if metrics.isEmpty then None
      else
        val visitorTotals = metrics.view.mapValues(_._4).toMap
        val avgProfits = metrics.view.mapValues(_._3).toMap

        val vList = visitorTotals.values.toList
        val pList = avgProfits.values.toList
        val minV = vList.min; val maxV = vList.max
        val minP = pList.min; val maxP = pList.max

        def vScore(v: Int): Double = if maxV == minV then 1.0 else (v - minV).toDouble / (maxV - minV)
        def pScore(p: Double): Double = if maxP == minP then 1.0 else (p - minP) / (maxP - minP)

        val scored = visitorTotals.keys.iterator.map { k =>
          val v = visitorTotals(k)
          val p = avgProfits(k)
          val score = (vScore(v) + pScore(p)) / 2.0
          (k, score)
        }.toList

        val (bestKey, bestScore) = scored.maxBy(_._2)
        val k = bestKey
        Some((s"${k.name}, ${k.city}, ${k.country}", bestScore))

    // Full profitable rankings with components (visitors, avgProfit, score)
    def profitableRankings(): List[(String, Int, Double, Double)] =
      val visitorTotals = metrics.view.mapValues(_._4).toMap
      val avgProfits = metrics.view.mapValues(_._3).toMap
      val vList = visitorTotals.values.toList
      val pList = avgProfits.values.toList
      val minV = vList.min; val maxV = vList.max
      val minP = pList.min; val maxP = pList.max
      def vScore(v: Int): Double = if maxV == minV then 1.0 else (v - minV).toDouble / (maxV - minV)
      def pScore(p: Double): Double = if maxP == minP then 1.0 else (p - minP) / (maxP - minP)
      visitorTotals.keys.iterator.map { k =>
        val v = visitorTotals(k)
        val p = avgProfits(k)
        val score = (vScore(v) + pScore(p)) / 2.0
        (s"${k.name}, ${k.city}, ${k.country}", v, p, score)
      }.toList.sortBy(-_._4)

  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage {}   // required for JFXApp

    println("=== Hotel Booking Analysis ===")

    val bookings = loadBookings("Hotel_Dataset.csv")

    if bookings.nonEmpty then
      val analytics = new HotelAnalytics(bookings)

      // Q1: concise summary with tie handling
      val (countries, cnt) = analytics.topCountries()
      println("\nQ1: Country(ies) with most bookings")
      if countries.isEmpty then
        println("-> (none)")
      else
        // Print primary winner (first in list) with count
        println(s"-> ${countries.head} ($cnt)")
        // If there are ties, print each tied country on its own line, otherwise print a no-ties line
        if countries.tail.nonEmpty then
          countries.tail.foreach(c => println(s"-> $c"))
        else
          println("------------  no ties  ----------------")

      // Q2: Most Economical Hotel
      val econRanks = analytics.economicalRankings()
      println("\nQ2: Most economical hotel(s)")
      if econRanks.isEmpty then
        println("-> (none)")
      else
        val topScore = econRanks.head._5
        val eps = 1e-12
        val winners = econRanks.filter { case (_, _, _, _, s) => math.abs(s - topScore) < eps }.map(_._1)
        // print first winner, then tied winners on separate lines or 'no ties' marker
        println(s"-> ${winners.head}")
        if winners.tail.nonEmpty then winners.tail.foreach(w => println(s"-> $w")) else println("------------  no ties  ----------------")

      // Q3: Most Profitable Hotel
      val profRanks = analytics.profitableRankings()
      println("\nQ3: Most profitable hotel(s)")
      if profRanks.isEmpty then
        println("-> (none)")
      else
        val topScoreP = profRanks.head._4
        val epsP = 1e-12
        val winnersP = profRanks.filter { case (_, _, _, s) => math.abs(s - topScoreP) < epsP }.map(_._1)
        println(s"-> ${winnersP.head}")
        if winnersP.tail.nonEmpty then winnersP.tail.foreach(w => println(s"-> $w")) else println("------------  no ties  ----------------")
    else
      println("ERROR: No data loaded.")


end MyApp

```


#### Short summary: 

empty definition using pc, found symbol in pc: scala/Option#