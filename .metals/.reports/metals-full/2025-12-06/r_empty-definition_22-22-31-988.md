error id: file:///C:/02%20PROJECTS/assingment-2-lani/src/main/scala/myfirstscala.scala:scala/Int#toDouble().
file:///C:/02%20PROJECTS/assingment-2-lani/src/main/scala/myfirstscala.scala
empty definition using pc, found symbol in pc: scala/Int#toDouble().
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -agg/bookings/toDouble.
	 -agg/bookings/toDouble#
	 -agg/bookings/toDouble().
	 -scala/Predef.agg.bookings.toDouble.
	 -scala/Predef.agg.bookings.toDouble#
	 -scala/Predef.agg.bookings.toDouble().
offset: 4921
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
  def parseCSVLine(line: String): Option[Array[String]] =
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
  case class HotelAgg(totalPrice: Double, totalPeople: Int, totalDiscount: Double, totalProfit: Double, bookings: Int, sumPricePerBookingPerson: Double):
    def +(o: HotelAgg) = HotelAgg(
      totalPrice + o.totalPrice,
      totalPeople + o.totalPeople,
      totalDiscount + o.totalDiscount,
      totalProfit + o.totalProfit,
      bookings + o.bookings,
      sumPricePerBookingPerson + o.sumPricePerBookingPerson
    )

  // Helper Function: Get core calculations shared across questions
  class HotelAnalytics(data: List[Booking]):

    // Map HotelAgg to HotelKey (Find totals for each hotel group)
    private val aggs: Map[HotelKey, HotelAgg] =
      data
        .groupMapReduce(b => HotelKey(b.hotelName, b.destinationCity, b.destinationCountry))(
          b => HotelAgg(
            b.bookingPrice,
            b.numPeople,
            b.discount,
            b.profitMargin,
            1,
            if b.numPeople > 0 then b.bookingPrice / b.numPeople else b.bookingPrice
          )
        )(_ + _)

    // Find average price, disocunt, profit and total people for that hotel group
    // Calculations for each hotel group so no need compute again for each question
    private lazy val metrics: Map[HotelKey, (Double, Double, Double, Int)] =
      aggs.view.mapValues { agg =>
        val avgPricePerPerson = if agg.bookings == 0 then 0.0 else agg.sumPricePerBookingPerson / agg.bookings.toDouble
        val avgDiscount = if agg.bookings == 0 then 0.0 else agg.totalDiscount / agg.bookings.toDo@@uble
        val avgProfit = if agg.bookings == 0 then 0.0 else agg.totalProfit / agg.bookings.toDouble
        val totalPeople = agg.totalPeople
        (avgPricePerPerson, avgDiscount, avgProfit, totalPeople)
      }.toMap

    // Q1: returns list of top country(ies) and the count
    def answerQ1(): (List[String], Int) =
      // map from destination country → booking count, map booking → 1, hence can sum those 1s by country to produce map
      val counts = data.groupMapReduce(_.destinationCountry)(_ => 1)(_ + _)
      if counts.isEmpty then (Nil, 0)
      else
        // Find the maximum count and collect all countries that reach it
        val maxCount = counts.values.max
        val top = counts.collect { case (k, v) if v == maxCount => k }.toList.sorted
        (top, maxCount)

    // Q2: returns list of most economical hotel name(s)
    def answerQ2(): List[String] =
      if metrics.isEmpty then Nil
      else
        // metrics contains per-hotel totals (avg price-per-booking, avg discount, avg profit).
        val priceVals = metrics.values.map(_._1).toList
        val discVals  = metrics.values.map(_._2).toList
        val profVals  = metrics.values.map(_._3).toList

        // compute min/max for min-max normalization
        val minP = priceVals.min; val maxP = priceVals.max
        val minD = discVals.min;  val maxD = discVals.max
        val minR = profVals.min;  val maxR = profVals.max
        def norm(v: Double, lo: Double, hi: Double): Double = if hi == lo then 1.0 else (v - lo) / (hi - lo)

        // average the three scores to get a single score
        val scored = metrics.iterator.map { case (k, (p, d, r, people)) =>
          val priceScore = 1.0 - norm(p, minP, maxP)
          val discScore  = norm(d, minD, maxD)
          val profScore  = 1.0 - norm(r, minR, maxR)
          val score = (priceScore + discScore + profScore) / 3.0
          (s"${k.name}, ${k.city}, ${k.country}", score)
        }.toList.sortBy(-_._2)

        // Sort hotels by score, handle ties
        if scored.isEmpty then Nil
        else
          val topScore = scored.head._2
          val eps = 1e-12
          scored.filter { case (_, s) => math.abs(s - topScore) < eps }.map(_._1)

    // Q3: returns list of most profitable hotel name(s)
    def answerQ3(): List[String] =
      if metrics.isEmpty then Nil
      else
        // metrics contains per-hotel totals (per-hotel total visitors and avg profit margin).
        val visitorTotals = metrics.view.mapValues(_._4).toMap
        val avgProfits = metrics.view.mapValues(_._3).toMap
        val vList = visitorTotals.values.toList
        val pList = avgProfits.values.toList

        // Min-max normalize visitor totals and avg profits independently into [0,1]
        val minV = vList.min; val maxV = vList.max
        val minP = pList.min; val maxP = pList.max

        // Get score for each criteria
        def vScore(v: Int): Double = if maxV == minV then 1.0 else (v - minV).toDouble / (maxV - minV)
        def pScore(p: Double): Double = if maxP == minP then 1.0 else (p - minP) / (maxP - minP)

        // Compute the average of the normalized visitor score and normalized profit score
        val scored = visitorTotals.keys.iterator.map { k =>
          val v = visitorTotals(k)
          val p = avgProfits(k)
          val score = (vScore(v) + pScore(p)) / 2.0
          (s"${k.name}, ${k.city}, ${k.country}", score)
        }.toList.sortBy(-_._2)

        // Sort by score, return top hotel, deal with ties
        if scored.isEmpty then Nil
        else
          val topScore = scored.head._2
          val eps = 1e-12
          scored.filter { case (_, s) => math.abs(s - topScore) < eps }.map(_._1)

  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage {}   // required for JFXApp

    println("=== Hotel Booking Analysis ===")

    val bookings = loadBookings("Hotel_Dataset.csv")

    if bookings.nonEmpty then
      val analytics = new HotelAnalytics(bookings)

      // Keep printing logic in a small helper to keep `start` clean
      def printWinnerWithTies(title: String, winners: List[String], countOpt: Option[Int] = None): Unit =
        println(s"\n$title")
        if winners.isEmpty then println("-> (none)")
        else
          println(s"-> ${winners.head}${countOpt.map(c => s" ($c)").getOrElse("")}")
          if winners.tail.nonEmpty then winners.tail.foreach(w => println(s"-> $w")) else println("------------  no ties  ----------------")

      // Q1
      val (countries, cnt) = analytics.answerQ1()
      printWinnerWithTies("Q1: Country(ies) with most bookings", countries, Some(cnt))

      // Q2
      val econWinners = analytics.answerQ2()
      printWinnerWithTies("Q2: Most economical hotel(s)", econWinners)

      // Q3
      val profWinners = analytics.answerQ3()
      printWinnerWithTies("Q3: Most profitable hotel(s)", profWinners)
    else
      println("ERROR: No data loaded.")


end MyApp

```


#### Short summary: 

empty definition using pc, found symbol in pc: scala/Int#toDouble().