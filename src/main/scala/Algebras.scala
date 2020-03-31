object Algebras {


  //Typesafe definition of a ring
  trait SimpleRing[U]{
    type +[+A<:U,+B<:U] = U //with A with B
    type *[A<:U,B<:U] = U

    //id[A]:A for any A<:U
    //+[A,id]:A
    //defines A as the type within U
    //such that + with any type
    // with identity produces A
    //this value should uniquely be U (up to
    // provable equivalence for any types within
    // the ring
    type idDef[A>:this.+[_,this.idDef[_]]] = A with U
    //def +[A<:U,B<:U](a:A,b:B):this.+[A,B]
    //def *[A<:U,B<:U](a:A,b:B):this.*[A,B]
    class Thing
    //    trait thing[A,B<:this.+[A,Id[_]]]{
    //      type +[_,B]= A
    //    }
    //type thing[A<:U,B<:this.+[A,Id[_]]] = +[A,Id[_]] with A

    type Id[A>:U] = idDef[A] with U

    //
    //type identity[A<:U] = Id[A] with thing[A,+[A,Id[A]]]
  }

  trait RingWithIdentity[U] extends SimpleRing[U]{
    //override type +[A<:U,B>:U] = A
  }

  trait SimpleRingType[U]{
    trait Join[-X]
    trait Meet[+X]
    type With[X,Y] = X with Y
    type +[A<:U,B<:U] = Join[A with B] with U
    type *[A<:U,B<:U] = Meet[A with  B] with U
    //id[A]:A for any A<:U
    //+[A,id]:A
    //defines A as the type within U
    //such that + with any type
    // with identity produces A
    //this value should uniquely be U (up to
    // provable equivalence for any types within
    // the ring
    type idDef[A>:this.+[_,this.idDef[_]]] = A with U
    type Id[A<:U] = idDef[U]
  }
  trait numb


  object ring extends RingWithIdentity[numb]
  import ring._
  //class num[A] extends numb with Id[A]
  //val ring = (new myRing)

  trait AAA extends numb//[AAA]
  trait BBB extends numb//[BBB]
  trait CCC extends numb//[CCC]
  //val testvar:AAA = null.asInstanceOf[+[AAA,BBB]]


}
