package test
import zio._
object ZlayerApp {
  def show(message:String):ZIO[Has[Show],Nothing,Has[Show]] = {
    ZIO.accessM[Has[Show]](_.get.display(message))
  }
//  def show(message:String):ZIO[Has[Show],Nothing,Has[Show]] = {
//    ZIO.accessM[Has[Show]](_.get.display(message))
//  }
  trait Show{
    def display(message:String):ZIO[Any,Nothing,Has[Show]]
  val valuetest:ZIO[Any,Nothing,Has[Double]]
  }

  val shower1:ZLayer[Any,Nothing,Has[Show]] = ZLayer.succeed(new Show {
    override def display(message: String): ZIO[Any, Nothing, Has[Show]] = ZIO.effectTotal({println(message);null.asInstanceOf[Has[Show]]})

    override val valuetest: ZIO[Any,Nothing,Has[Double]] = ZIO.effectTotal(Has(1d))
  })

  val withlayer1 = (for{
    p1 <- show("hello")
    pw <- show("world")
    //p2 <-  p1.get.valuetest
    //p3 <- show(s"${p2}")
  }yield pw).provideLayer(shower1)
  //show("hello").flatMap(_ => show("world")).provideLayer(shower1)

  def main(args:Array[String]):Unit = {
    Runtime.default.unsafeRun(withlayer1)
  }

}
