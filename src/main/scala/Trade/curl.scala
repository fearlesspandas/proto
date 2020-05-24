package Trade

import java.io.{File, FileOutputStream, PrintWriter}

import Trade.curl.signature
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import scala.io.Source
import Typical.impl._
import Typical.core.Typeable.dataset
import Typical.implicits.implicits._

import scala.reflect.ClassTag
object curl {

  import sys.process._

  val tokenfile = Source.fromFile("src/main/resources/oauth")
  val secretfile = Source.fromFile("src/main/resources/secret")
  val passphrasefile = Source.fromFile("src/main/resources/passphrase")

  val bearertoken = tokenfile.getLines().foldLeft("")(_ + _)
  val secret = secretfile.getLines().foldLeft("")(_ + _)
  val decodedsecret = java.util.Base64.getDecoder().decode(secret)
  val passphrase = passphrasefile.getLines().foldLeft("")(_ + _)
  val signature = (urlpath:String,timestamp:Long) => generateHMAC(decodedsecret.toString,s"$timestamp" + "GET" + urlpath)

  tokenfile.close()
  secretfile.close()
  passphrasefile.close()


  class curl[self<:curl[self]](
              outpath:String,
              path:String,
              bearertoken:String,
              passphrase:String,
              append:Boolean
            )(implicit tag:ClassTag[self]) extends rsim[String, self, self](
    ((src: dataset[self]) => {
      val timestamp = System.currentTimeMillis()/1000
      val sig = signature(bookpath,timestamp)
      val cmd = Seq("curl", "https://api.pro.coinbase.com" + path,
        "-H", "Content-Type: application/json",
        "-H", s"CB-ACCESS-KEY:$bearertoken",
        "-H", s"CB-ACCESS-SIGN:$sig",
        "-H", s"CB-ACCESS-TIMESTAMP: $timestamp",
        "-H", s"CB-ACCESS-PASSPHRASE: $passphrase"
      )
      try{
        val pw = new PrintWriter(new FileOutputStream(outpath,append))
        val res = cmd.!!
        if (append) pw.append(res) else pw.write(res)
        pw.close()
      }catch{
        case e:Exception => e.printStackTrace()
      }
      outpath
    }).set[self]
  )("")


  val bookpath = "/products/LTC-USD/book?level=2"
  val bidoutpath = "src/main/resources/bidout.json"
  class bookcurl extends curl[bookcurl](bidoutpath,bookpath,bearertoken,passphrase,false)


  val tickerpath = "/products/LTC-USD/ticker"
  val tickeroutpath = "src/main/resources/tickerout.json"
  class tickercurl extends curl[tickercurl](tickeroutpath,tickerpath,bearertoken, passphrase,true)


  def generateHMAC(sharedSecret: String, preHashString: String): String = {
    val secret = new SecretKeySpec(sharedSecret.getBytes, "SHA256")
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(secret)
    val hashString: Array[Byte] = mac.doFinal(preHashString.getBytes)
    new String(hashString.map(_.toChar))
  }
}