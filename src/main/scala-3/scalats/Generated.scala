package scalats

import cats.Monoid
import cats.syntax.semigroup.*

case class Generated(imports: TsImports, code: String) {
  def |+|(other: Generated): Generated =
    Generated(TsImports.monoid.combine(imports, other.imports), code + other.code)

  def |+|(other: String): Generated =
    Generated(imports, code + other)
}

object Generated {
  implicit val monoid: Monoid[Generated] =
    Monoid.instance(Generated(TsImports.monoid.empty, ""), _ |+| _)

  lazy val empty: Generated = monoid.empty
}
