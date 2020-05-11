import java.io.{ByteArrayOutputStream, File, PrintWriter}

import Typical.core.Typeable._
import Typical.impl._

import scala.reflect.ClassTag
import Typical.implicits.implicits._
import org.apache.spark.sql._
import org.apache.spark.sql.functions._

import scala.io.Source
object Test {

  import sys.process._
  val timestamp = System.currentTimeMillis()/1000
  val path = "/products/LTC-USD/book?level=2"
  val bearertoken = Source.fromFile("src/main/resources/oauth").getLines().foldLeft("")(_ + _)
  val secret = Source.fromFile("src/main/resources/secret").getLines().foldLeft("")(_ + _)
  val decodedsecret = java.util.Base64.getDecoder().decode(secret)
  val passphrase = Source.fromFile("src/main/resources/passphrase").getLines().foldLeft("")(_ + _)
  val signature = generateHMAC(decodedsecret.toString,s"$timestamp" + "GET" + path)
  //val sig_64 = java.util.Base64.getEncoder().encode(signature.getBytes())//.toString()
  //println(decodedsecret)

  class curl extends recSim[String,curl,curl](
    ((src:dataset[curl]) => {
        val cmd = Seq("curl","https://api.pro.coinbase.com" + path,
          "-H","Content-Type: application/json",
          "-H",s"CB-ACCESS-KEY:$bearertoken",
          "-H",s"CB-ACCESS-SIGN:$signature",
          "-H",s"CB-ACCESS-TIMESTAMP: $timestamp",
          "-H",s"CB-ACCESS-PASSPHRASE: $passphrase"
        )
      val pw = new PrintWriter(new File("src/main/resources/bidout.json"))
      val res = cmd.!!
      pw.write(res)
      pw.close()
      res
    }).set[curl]
  )("")
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
      }).toDF("price","volume","quantity")//.withColumnRenamed("value","bid_price")
    }).set[biddf]
  )(Seq.empty[Int].toDF)

  class askdf extends recSim[DataFrame,askdf, askdf with curl](
    ((src:dataset[askdf with curl]) => {
      val data = src.calc[String,curl].typedInitVal
      val rawdf = spark.read.json("src/main/resources/bidout.json")
      rawdf.withColumn("asks", explode(rawdf.col("asks"))).select($"asks").map((row:Row) => {
        (
          row.getAs[Seq[String]](0)(0).toDouble,
          row.getAs[Seq[String]](0)(1).toDouble,
          row.getAs[Seq[String]](0)(2).toDouble
        )
      }).toDF("price","volume","quantity")//.withColumnRenamed("value","bid_price")
    }).set[askdf]
  )(Seq.empty[Int].toDF)
  import javax.crypto.Mac
  import javax.crypto.spec.SecretKeySpec

    def generateHMAC(sharedSecret: String, preHashString: String): String = {
      val secret = new SecretKeySpec(sharedSecret.getBytes, "SHA256")   //Crypto Funs : 'SHA256' , 'HmacSHA1'
      val mac = Mac.getInstance("HmacSHA256")
      mac.init(secret)
      val hashString: Array[Byte] = mac.doFinal(preHashString.getBytes)
      new String(hashString.map(_.toChar))
    }
  def runCommand(cmd: Seq[String]): (Int, String, String) = {
    val stdoutStream = new ByteArrayOutputStream
    val stderrStream = new ByteArrayOutputStream
    val stdoutWriter = new PrintWriter(stdoutStream)
    val stderrWriter = new PrintWriter(stderrStream)
    val exitValue = cmd.!(ProcessLogger(stdoutWriter.println, stderrWriter.println))
    stdoutWriter.close()
    stderrWriter.close()
    (exitValue, stdoutStream.toString, stderrStream.toString)
  }

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
    res.coalesce(1).write.option("mode","overwrite").csv("src/main/resources/ltc-pricedata-bids")
    res2.coalesce(1).write.option("mode","overwrite").csv("src/main/resources/ltc-pricedata-asks")
    println(s"Result: $res")
    res.show(false)
    res2.show(false)
    println(s"bidcount:${res.count}")
    println(s"askcount:${res2.count}")
    val t1 = System.nanoTime()

    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}