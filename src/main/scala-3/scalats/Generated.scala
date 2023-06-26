package scalats

import cats.Monoid

/** Generated TypeScript code, including a set of [[scalats.TsImports]] and the code itself as a `String` */
final case class Generated(imports: TsImports, code: String) {
  def |+|(other: Generated): Generated =
    Generated(TsImports.monoid.combine(imports, other.imports), code + other.code)

  def |+|(other: String): Generated =
    Generated(imports, code + other)
}

object Generated {
  /** Convert a `String` requiring no imports to a [[scalats.Generated]] */
  def lift(s: String): Generated = Generated(TsImports.empty, s)

  /** Empty generated code */
  val empty: Generated = lift("")

  given monoid: Monoid[Generated] = Monoid.instance(empty, _ |+| _)
}
