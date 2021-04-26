package Grammar

import Typical.core.typeable.{contexttype, dataset, idtype, model}

class TMap[dependencies<:dataset[_],mapper<:model[dependencies,_>:dataset[dependencies]<:dataset[_]]](dep:dataset[dependencies], f:mapper) extends dataset[dependencies with TMap[_,mapper]] {
  override val context: contexttype = dep.context
  val dependency = dep
  override def withContext(ctx: contexttype): dataset[dependencies  with TMap[_,mapper]] = new TMap(dep,f) {
    override val context = ctx
  }

  override val id: idtype = null.asInstanceOf[idtype]

  //override val value: Any = null
  override val IdRelations: Map[idtype, idtype] = dep.IdRelations
  //override val errorMap: Map[idtype, Error] = dep.errorMap
}
object TMap{
  def apply[dependencies<:dataset[_],mapper<:model[dependencies,_>:dataset[dependencies]<:dataset[_]]](dep:dataset[dependencies],f:mapper):TMap[dependencies,mapper] = new TMap[dependencies,mapper](dep,f)
}

