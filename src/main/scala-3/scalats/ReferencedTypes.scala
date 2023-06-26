package scalats

import cats.Monoid
import cats.syntax.semigroup.*

/**
 * A mapping of [[scalats.TypeName]] to the [[scalats.TypeName]]s it refers to
 *
 * Used during TypeScript code generation to ensure type definitions are generated in the correct order.
 *
 * @see [[scalats.TypeSorter]]
 */
final class ReferencedTypes private[scalats] (private[ReferencedTypes] val types: Map[TypeName, Set[TypeName]])  {
  private lazy val typesByName = types.map { case (t, ts) => (t.full, ts) }
  final def get(typeName: TypeName): Option[Set[TypeName]] = typesByName.get(typeName.full)
}

object ReferencedTypes {
  val empty = new ReferencedTypes(Map.empty)

  given monoid: Monoid[ReferencedTypes] = Monoid.instance(empty, (a, b) => new ReferencedTypes(a.types |+| b.types))
}
