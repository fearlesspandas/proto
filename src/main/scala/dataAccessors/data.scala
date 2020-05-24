package dataAccessors

import Trade.curl.{bookcurl, tickercurl}
import Typical.core.Typeable.dataset
import org.apache.spark.sql.functions.explode
import org.apache.spark.sql.{DataFrame, Row, SparkSession}
import Typical.implicits.implicits._
import Typical.impl._
object data {
  val spark = SparkSession.builder.master("local[*]").getOrCreate()
  import spark.implicits._

  class price extends axiom[String,price]("price")
  class volume extends axiom[String,volume]("volume")
  class quantity extends axiom[String,quantity]("quantity")
  class bid extends axiom[String,bid]("bid")
  class ask extends axiom[String,ask]("ask")

  class biddf extends rsim[DataFrame, biddf with bookcurl with price with volume with quantity,biddf](
    ((src:dataset[biddf with bookcurl with price with volume with quantity]) => {
      val data = src.calc[String,bookcurl].value
      val rawdf = spark.read.json(data)
      rawdf.withColumn("bids", explode(rawdf.col("bids"))).select($"bids").map((row:Row) => {
        (
          row.getAs[Seq[String]](0)(0).toDouble,
          row.getAs[Seq[String]](0)(1).toDouble,
          row.getAs[Seq[String]](0)(2).toDouble
        )
      }).toDF(
        src.fetch[String,price].value,
        src.fetch[String,volume].value,
        src.fetch[String,quantity].value
      )
    }).set[biddf]
  )(Seq.empty[Int].toDF)

  class askdf extends rsim[DataFrame, askdf with bookcurl with price with volume with quantity,askdf](
    ((src:dataset[askdf with bookcurl with price with volume with quantity]) => {
      val data = src.calc[String,bookcurl].value
      val rawdf = spark.read.json(data)
      rawdf.withColumn("asks", explode(rawdf.col("asks"))).select($"asks").map((row:Row) => {
        (
          row.getAs[Seq[String]](0)(0).toDouble,
          row.getAs[Seq[String]](0)(1).toDouble,
          row.getAs[Seq[String]](0)(2).toDouble
        )
      }).toDF(
        src.fetch[String,price].value,
        src.fetch[String,volume].value,
        src.fetch[String,quantity].value
      )
    }).set[askdf]
  )(Seq.empty[Int].toDF)

  class tickerdf extends rsim[DataFrame,tickercurl with tickerdf,tickerdf](
    ((src:dataset[tickerdf with tickercurl]) => {
      val tickerout = src.calc[String,tickercurl].value
      spark.read.json(tickerout).distinct()
    }).set[tickerdf]
  )(Seq.empty[Int].toDF)


  class enrichedData extends rsim[DataFrame,tickerdf with enrichedData with tickercurl,enrichedData](
    ((src:dataset[tickerdf with enrichedData with tickercurl]) => {
      val df = src.calc[DataFrame,tickerdf].value.sort($"time".desc)
//      val bidcol = src.fetch[String,bid].value
//      val askcol = src.fetch[String,ask].value
      df
    }).set[enrichedData]
  )(null)
}
