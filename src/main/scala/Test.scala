import java.io.{ByteArrayOutputStream, File, PrintWriter}

import Typical.core.Typeable._
import Typical.impl._

import Typical.implicits.implicits._
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import Trade.curl._
import dataAccessors.data._
object Test {



  def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime()
    val dat = data[tickercurl with tickerdf with enrichedData](myprovider.register[tickercurl].register[tickerdf].register[enrichedData])
    val updateddata = (0 to 2).foldLeft[dataset[tickercurl with tickerdf with enrichedData]](dat)((data,curr) => {Thread.sleep(1000);data.calc[String,tickercurl]} )
    val res = updateddata.calc[DataFrame,enrichedData].typedInitVal
    println(s"Result:")
    res.show(false)
    val t1 = System.nanoTime()

    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }

  def main1(args:Array[String]): Unit = {
    val spark = SparkSession.builder.master("local[*]").getOrCreate()
    val t0 = System.nanoTime()
    val dat = data[biddf with bookcurl with askdf with price with volume with quantity](
      myprovider
        .register[biddf]
        .register[bookcurl]
        .register[askdf]
        .register[price]
        .register[volume]
        .register[quantity]
    )
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