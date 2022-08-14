package Typical.core
object Util {


  ////state tree
  ///   every dataset has an id
  ///   core operations have an id
  ///   when we  perform an operation we can check if there is a cached state
  ///     which already exists under that combination of id's

  trait Tree[+A]{
    val parent:Option[Tree[A]]
    val children:Set[Tree[_<:A]]
    val id:Long
    val context:A

    def apply[B](ctx:B,parent:Option[Tree[B]],children:Set[Tree[B]]):Tree[B] = {
      val id = ???
      apply(id,ctx,parent,children)
    }
    def apply[B](id:Long,ctx:B,parent:Option[Tree[B]],children:Set[Tree[B]]):Tree[B]
    def add_child(ctx:_<:A):Tree[A] = {
      apply[A](
        this.id,
        this.context,
        this.parent,
        this.children ++ apply[A](ctx,Some(this),Set())
      )
    }
    def add_child_make_head(ctx:_<:A):Tree[A] = {
      apply[A](ctx,Some(this),Set())
    }
  }
}
