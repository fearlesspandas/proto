import Typical.core.Typeable._
import Typical.impl._
import scala.reflect.ClassTag
import Typical.implicits.implicits._
import org.apache.spark.sql._
object Test {

  /**
   * In this example we illustrate how Typical can be used to easily encapsulate transormations
   * on Spark dataframes in a typesafe way. We define our dataframe transormation within
   * a Typical calculation, including the columns relevent to the calculation as dependencies.
   * As with all calculations in the Typical grammar, this allows us to know at compile time
   * whether our total transformations structure has all the relevent dependencies at every step
   * from start to finish.
   *
   * Important Note!! In this example, joins cannot be used arbitrarily
   * within calculations, but instead must be encapsulated in a type, as a dependency to be safely
   * used. This is because wrapping the desired join into a type is the only way to prove
   * at compile time, that in any context, our dataframes which get joined contain the relevent
   * join column. Towards the end of this example we will show how to do this.
   *
   *
   *
   * We start by retrieving our spark session
   */
  val spark = SparkSession.builder().config("spark.master","local").getOrCreate()
  import spark.implicits._

  /**
   * We will need a source dataframe as an initial value of our calculation
   */
  val srcdf = Seq((1,2)).toDF("col1","col2")

  /**
   * We then match our axioms to our columns
   */
  case class col1() extends axiom[Column,col1]( srcdf.col("col1"))
  case class col2() extends axiom[Column,col2]( srcdf.col("col2"))

  /**
   * Define our dataframe transformation inside our Typical calculation function
   * with dependencies DF with col1 with col2
   */
  val DF_iter1 = ((src:dataset[DF with col1 with col2]) => {
    //access our data in a typesafe way from our dependency set
    val df = src.fetch[DataFrame,DF].typedInitVal
    val one = src.fetch[Column,col1]
    val two = src.fetch[Column,col2]
    //here we can use spark as we normally would as we have already
    //retrieved our values in a typesafe way
    //The only operation not guarenteed to be typesafe
    //would be join, as we did not include a join type
    //as a dependency
    df.withColumn(two.name,df.col(one.name) + df.col(two.name))
  })
  val DF_iter2 = ((src:dataset[DF2 with col1 with col2]) => {
    //access our data in a typesafe way from our dependency set
    val df = src.fetch[DataFrame,DF2].typedInitVal
    val one = src.fetch[Column,col1]
    val two = src.fetch[Column,col2]
    //here we can use spark as we normally would as we have already
    //retrieved our values in a typesafe way
    //The only operation not guarenteed to be typesafe
    //would be join, as we did not include a join type
    //as a dependency
    df.withColumn(two.name,df.col(one.name) + df.col(two.name))
  })

  val df1_func = DF_iter1.set[DF]
  val df2_func = DF_iter2.set[DF2]

  /**
   * Note: In the above example calculation we use the columns from our src df with names
   * matching our dependencies, rather than using the retrieved columns themselves in
   * the new column definition. The reason for this is,as implemented, the actual values for our
   * column dependencies in Typicals statestore are never updated. However, by using the
   * names of our dependencies to retrieve the relevent columns in DF, as implemented it
   * still solves the problem of typesafe data access for a dataframe transformation
   * so long as src is the only data used in the result of our calculations.
   *
   */

  /**
   * Define our dataframe type and bind our dependencies to it (it's a recursive sim
   * so itself needs to be included as a dependency)
   */
  class DF extends recSim[DataFrame,DF,col1 with col2 with DF](df1_func)(srcdf)
  class DF2 extends recSim[DataFrame,DF2,col1 with col2 with DF2](df2_func)(srcdf)

  /**
   * Example of how to build a join type using Typical.impl.join
   * on DF and DF2 on col1. To be used directly in a transformation
   * we would include this type as a dependency.
   *
   */
  class jointest extends join[jointest,col1,DF,DF2]
  /**
   * Prepare the context provider with the initial values
   */
  implicit val ctx = myprovider
    .register[col1]
    .register[col2]
    .register[DF]
    .register[DF2]

  /**
   * We then build our data with the dependencies we want to include, and our context,
   * and run calc on DF and DF2 any number of times we want to iterate their transformations.
   */
  lazy val dfdat = data[DF with col1 with col2 with DF2 with jointest](ctx).calc[DataFrame,DF2].calc[DataFrame,DF].calc[DataFrame,DF]

  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder().config("spark.master","local").getOrCreate()
    val t0 = System.nanoTime()
    dfdat.typedInitVal.show
    dfdat.fetch[DataFrame,DF2].typedInitVal.show
    dfdat.calc[DataFrame,jointest].typedInitVal.show
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}