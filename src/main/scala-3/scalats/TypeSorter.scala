package scalats

import cats.Eval

object TypeSorter {
  def sort(types: List[(Option[TypeName], Generated)], refs: ReferencedTypes): List[Generated] = {
    val all = types.flatMap { case (typeName, generated) => typeName.map(n => (n.full, generated)) }.toMap

    def addType(
      acc: Eval[(List[Generated], Set[String])],
      typeName: TypeName,
      generated: Generated,
    ): Eval[(List[Generated], Set[String])] =
      acc.flatMap { case (accGen, accSkip) =>
        if (accSkip.contains(typeName.full))
          Eval.now((accGen, accSkip))
        else
          refs.get(typeName) match {
            case Some(types) =>
              types.foldLeft(Eval.now((accGen, accSkip + typeName.full)))(
                (acc, ref) => all.get(ref.full).fold(acc)(addType(acc, ref, _))
              ).map { case (x, y) => (x :+ generated, y) }
            case None =>
              Eval.now((accGen :+ generated, accSkip + typeName.full))
          }
      }

    types.foldLeft(Eval.now(List.empty[Generated], Set.empty[String])) {
      case (acc, (Some(typeName), generated)) => addType(acc, typeName, generated)
      case (acc, (None, generated)) => acc.map { case (accGen, accSkip) => (accGen :+ generated, accSkip) }
    }.value._1
  }
}
