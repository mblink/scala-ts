package scalats

import cats.Monoid
import cats.syntax.semigroup.*

final class ReferencedTypes private (private[ReferencedTypes] val types: Map[TypeName, Set[TypeName]])  {
  private lazy val typesByName = types.map { case (t, ts) => (t.full, ts) }
  final def get(typeName: TypeName): Option[Set[TypeName]] = typesByName.get(typeName.full)
}

object ReferencedTypes {
  val empty = new ReferencedTypes(Map.empty)
  def apply(types: Map[TypeName, Set[TypeName]]): ReferencedTypes = new ReferencedTypes(types)

  given monoid: Monoid[ReferencedTypes] = Monoid.instance(empty, (a, b) => ReferencedTypes(a.types |+| b.types))
}
