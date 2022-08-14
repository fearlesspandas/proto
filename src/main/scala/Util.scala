package Typical.core
object Util {

  ////state tree
  ///   every dataset has an id
  ///   core operations have an id
  ///   when we  perform an operation we can check if there is a cached state
  ///     which already exists under that combination of id's

  trait Tree[+A] {
    val parent: Option[Tree[A]]
    val children: Seq[Tree[A]]
    val id: Long
    val context: A

    def apply[B](id: Long, ctx: B, parent: Option[Tree[B]], children: Seq[Tree[B]]): Tree[B]
    def add_child[B>:A](ctx: B): Tree[B] =
      apply[B](
        this.id,
        this.context,
        this.parent,
         Seq(
          apply[B](
            this.id + 1,
            ctx,
            Some(this),
            Seq.empty[Tree[A]]
          )
        ) ++ this.children

      )
    def add_child_make_head[B>:A](ctx: B): Tree[B] =
      apply[B](this.id + 1, ctx, Some(this.add_child[B](ctx)), Seq.empty[Tree[A]])
  }

  case class TreeNode[+A](id: Long, parent: Option[Tree[A]], children: Seq[Tree[A]], context: A)
      extends Tree[A] {
    override def apply[B](
      id: Long,
      ctx: B,
      parent: Option[Tree[B]],
      children: Seq[Tree[B]]
    ): Tree[B] =
      TreeNode[B](id, parent, children, ctx)

    override def toString: String = s"Tree $id"
  }
}
