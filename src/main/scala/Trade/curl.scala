package Trade

import java.io.{File, PrintWriter}

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import scala.io.Source

import Typical.impl._

import Typical.core.Typeable.dataset
import Typical.impl.recSim
import Typical.implicits.implicits._
object curl {

  import sys.process._
  val timestamp = System.currentTimeMillis()/1000
  val path = "/products/LTC-USD/book?level=2"
  val bearertoken = Source.fromFile("src/main/resources/oauth").getLines().foldLeft("")(_ + _)
  val secret = Source.fromFile("src/main/resources/secret").getLines().foldLeft("")(_ + _)
  val decodedsecret = java.util.Base64.getDecoder().decode(secret)
  val passphrase = Source.fromFile("src/main/resources/passphrase").getLines().foldLeft("")(_ + _)
  val signature = generateHMAC(decodedsecret.toString,s"$timestamp" + "GET" + path)



  class curl extends recSim[String, curl, curl](
    ((src: dataset[curl]) => {
      val cmd = Seq("curl", "https://api.pro.coinbase.com" + path,
        "-H", "Content-Type: application/json",
        "-H", s"CB-ACCESS-KEY:$bearertoken",
        "-H", s"CB-ACCESS-SIGN:$signature",
        "-H", s"CB-ACCESS-TIMESTAMP: $timestamp",
        "-H", s"CB-ACCESS-PASSPHRASE: $passphrase"
      )
      val pw = new PrintWriter(new File("src/main/resources/bidout.json"))
      val res = cmd.!!
      pw.write(res)
      pw.close()
      res
    }).set[curl]
  )("")


  def generateHMAC(sharedSecret: String, preHashString: String): String = {
    val secret = new SecretKeySpec(sharedSecret.getBytes, "SHA256")   //Crypto Funs : 'SHA256' , 'HmacSHA1'
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(secret)
    val hashString: Array[Byte] = mac.doFinal(preHashString.getBytes)
    new String(hashString.map(_.toChar))
  }
}