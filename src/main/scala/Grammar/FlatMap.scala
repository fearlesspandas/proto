package Grammar

import Typical.core.typeable.{TerminalType, contexttype, dataset, idtype, model}

class FlatMap[dependencies<:dataset[_],prog<:model[dependencies,prog] with TerminalType[_>:dataset[dependencies]<:dataset[_]]](val dep:dataset[dependencies], out:prog) extends dataset[dependencies with FlatMap[dependencies,prog]] {
  override val context: contexttype = dep.context
  override def withContext(ctx: contexttype): FlatMap[dependencies,prog] = new FlatMap[dependencies,prog](dep,out) {
    override val context = ctx
  }

  override def id: idtype = null.asInstanceOf[idtype]

  override val value: Any = null
}
object FlatMap{
  def apply[dependencies<:dataset[_],prog<:model[dependencies,prog] with TerminalType[_>:dataset[dependencies]<:dataset[_]]](dep:dataset[dependencies], f:prog):FlatMap[dependencies,prog] = new FlatMap[dependencies,prog](dep,f)
}
