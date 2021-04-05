package Grammar

import Typical.core.typeable.{contexttype, dataset, idtype, model}

class TMap[dependencies<:dataset[_],mapper<:model[dependencies,_>:dataset[dependencies]<:dataset[_]]](dep:dataset[dependencies], f:mapper) extends dataset[dependencies with mapper with TMap[dependencies,mapper]] {
  override val context: contexttype = dep.context
  val dependency = dep
  override def withContext(ctx: contexttype): dataset[dependencies with mapper with TMap[dependencies,mapper]] = new TMap(dep,f) {
    override val context = ctx
  }

  override def id: idtype = null.asInstanceOf[idtype]

  override val value: Any = null
}
object TMap{
  def apply[dependencies<:dataset[_],mapper<:model[dependencies,_>:dataset[dependencies]<:dataset[_]]](dep:dataset[dependencies],f:mapper):TMap[dependencies,mapper] = new TMap(dep,f)
}

