package Typical.core
object Util {


  ////state tree
  ///   every dataset has an id
  ///   core operations have an id
  ///   when we  perform an operation we can check if there is a cached state
  ///     which already exists under that combination of id's

  trait Tree[+A]{
    val parent:Option[Tree[Any]]
    val children:Set[Tree[Any]]
    val id:Long
    val context:A

    def applyNoId[B](ctx:B,parent:Option[Tree[Any]],children:Set[Tree[Any]]):Tree[B] = {
      val id = 999
      apply[B](id,ctx,parent,children)
    }
    def apply[B](id:Long,ctx:B,parent:Option[Tree[Any]],children:Set[Tree[Any]]):Tree[B]
    def add_child[B](ctx:B):Tree[A] = {
      apply[A](
        this.id + 1,
        this.context,
        this.parent,
        this.children ++ Set(apply[B](this.id + 1,ctx,Some(this),Set()))
      )
    }
    def add_child_make_head[B](ctx:B):Tree[B] = {
      apply[B](this.id + 1,ctx,Some(this.add_child(ctx)),Set())
    }
  }

  case class TreeNode[+A](id:Long,parent:Option[Tree[Any]],children:Set[Tree[Any]],context:A) extends Tree[A]{
    override def apply[B](id:Long,ctx:B,parent:Option[Tree[Any]],children:Set[Tree[Any]]):Tree[B] = {
      TreeNode[B](id,parent,children,ctx)
    }
  }
}
