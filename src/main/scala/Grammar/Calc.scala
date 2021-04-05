package Grammar

import Typical.core.typeable.{contexttype, dataset, idtype, model}

class Calc[dependencies<:dataset[_],output<:model[dependencies,output]](dep:dataset[dependencies],out:output) extends dataset[dependencies with output with Calc[dependencies,output]] {
  override val context: contexttype = dep.context
  val dependency = dep
  override def withContext(ctx: contexttype): dataset[dependencies with output with Calc[dependencies,output]] = new Calc(dep,out) {
    override val context = ctx
  }

  override def id: idtype = null.asInstanceOf[idtype]

  override val value: Any = null
}
object Calc{
  def apply[dependencies<:dataset[_],output<:model[dependencies,output]](dep:dataset[dependencies],out:output):Calc[dependencies,output] = new Calc(dep,out)
}

