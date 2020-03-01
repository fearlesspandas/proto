import Typical.core.Typeable._
import org.apache.spark.sql.{Column, SparkSession}
import org.apache.spark.sql.functions.{lit, when}

object Test {

  val spark = SparkSession.builder().getOrCreate()
  import spark.implicits._

//  implicit val df = Seq((1,2)).toDF("one","two")
//
//  class one extends Ax[one]
//
//  val f : one => Column = (o:one) => o.func(null).plus(1)
//  val coldefs = Seq( new Col[one]( f))
//
//  val res = iterate(iterate(df,coldefs:_*),coldefs:_*)
}