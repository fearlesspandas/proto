import Typical.core.Typeable._
import Typical.impl._
import scala.reflect.ClassTag
import Typical.implicits.implicits._
object Test {

  class clientage extends axiom[Option[Int],clientage](Some(0))
  class clientname extends axiom[Option[String],clientname](Some("bob"))

  case class person(age:Int,name:String)
  class clientvalidator extends sim[Option[person],clientage with clientname,clientvalidator](
    ((src:dataset[clientage with clientname]) => {
      val ageOpt = src.fetch[Option[Int],clientage].typedInitVal
      val clientnameOpt = src.fetch[Option[String],clientname].typedInitVal
      (ageOpt,clientnameOpt) match {
        case (Some(age),Some(name)) => Some(person(age,name))
        case _ => None
      }
    }).set[clientvalidator]
  )(None)

  case class insurance(insured:person)
  class InsuranceValidator extends rsim[Option[insurance],InsuranceValidator with clientvalidator with clientage with clientname,InsuranceValidator](
    ((src:dataset[InsuranceValidator with clientvalidator with clientage with clientname]) => {
      src.calc[Option[person],clientvalidator].typedInitVal match{
        case Some(prsn) => Some(insurance(prsn))
        case _ => None
      }
    }).set[InsuranceValidator]
  )(None)

  val dat = data[clientage with clientname with clientvalidator with InsuranceValidator](
    myprovider
      .register[clientage]
      .register[clientname]
      .register[clientvalidator]
      .register[InsuranceValidator]
  )
  def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime()
    println(dat.calc[Option[insurance],InsuranceValidator].typedInitVal)
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}