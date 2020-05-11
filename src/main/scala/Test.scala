import java.io.{ByteArrayOutputStream, File, PrintWriter}

import Typical.core.Typeable._
import Typical.impl._

import Typical.implicits.implicits._
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import Trade.curl._

object Test {


  val spark = SparkSession.builder.master("local[*]").getOrCreate()
  import spark.implicits._

  class biddf extends recSim[DataFrame,biddf, biddf with curl](
    ((src:dataset[biddf with curl]) => {
      val data = src.calc[String,curl].typedInitVal
      val rawdf = spark.read.json("src/main/resources/bidout.json")
      rawdf.withColumn("bids", explode(rawdf.col("bids"))).select($"bids").map((row:Row) => {
        (
          row.getAs[Seq[String]](0)(0).toDouble,
          row.getAs[Seq[String]](0)(1).toDouble,
          row.getAs[Seq[String]](0)(2).toDouble
        )
      }).toDF("price","volume","quantity")
    }).set[biddf]
  )(Seq.empty[Int].toDF)

  class askdf extends recSim[DataFrame,askdf, askdf with curl](
    ((src:dataset[askdf with curl]) => {
      val data = src.calc[String,curl].typedInitVal
      val rawdf = spark.read.json(data)
      rawdf.withColumn("asks", explode(rawdf.col("asks"))).select($"asks").map((row:Row) => {
        (
          row.getAs[Seq[String]](0)(0).toDouble,
          row.getAs[Seq[String]](0)(1).toDouble,
          row.getAs[Seq[String]](0)(2).toDouble
        )
      }).toDF("price","volume","quantity")
    }).set[askdf]
  )(Seq.empty[Int].toDF)


  def main1(args: Array[String]): Unit = {
    val t0 = System.nanoTime()
    val dat = data[curl](myprovider.register[curl])
    val res = dat.calc[String,curl].typedInitVal
    println(s"Result: $res")
    val t1 = System.nanoTime()

    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }

  def main(args:Array[String]): Unit = {
    val spark = SparkSession.builder.master("local[*]").getOrCreate()
    val t0 = System.nanoTime()
    val dat = data[biddf with curl with askdf](myprovider.register[biddf].register[curl].register[askdf])
    val res = dat.calc[DataFrame,biddf].typedInitVal
    val res2 = dat.calc[DataFrame,askdf].typedInitVal
    //res.coalesce(1).write.option("mode","overwrite").csv("src/main/resources/ltc-pricedata-bids")
    //res2.coalesce(1).write.option("mode","overwrite").csv("src/main/resources/ltc-pricedata-asks")
    println(s"Result: $res")
    res.show(false)
    res2.show(false)
    println(s"bidcount:${res.count}")
    println(s"askcount:${res2.count}")
    val t1 = System.nanoTime()

    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}