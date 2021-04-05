package Grammar

import Typical.core.typeable.{contexttype, dataset, idtype, model}
/*
  This class encapsulates the 'calc' operation as a type, meaning if a dataset X contains a type Calc[A,U] then at some point in the history of X's
  state construction, the operation dataset[A].calc[U] was performed. Calc types can be ignored completely and be seen as simply a closure over the
  paradigm of binding the concepts of types-values-operations. Alternatively one can actually make use of Calc types by including them in the
  dependencies of models, which amounts to requiring that , for a dependency Calc[A,U] on type C, if we want to perform dat.calc[C] then dat
  must already have performed operation dat.calc[U] for dat.calc[C] to be valid.
 */
class Calc[dependencies<:dataset[_],output<:model[dependencies,output]](val dep:dataset[dependencies],out:output) extends dataset[dependencies with output with Calc[dependencies,output]] {
  override val context: contexttype = dep.context
  override def withContext(ctx: contexttype): dataset[dependencies with output with Calc[dependencies,output]] = new Calc(dep,out) {
    override val context = ctx
  }

  override def id: idtype = null.asInstanceOf[idtype]

  override val value: Any = null
}
object Calc{
  def apply[dependencies<:dataset[_],output<:model[dependencies,output]](dep:dataset[dependencies],out:output):Calc[dependencies,output] = new Calc(dep,out)
}

