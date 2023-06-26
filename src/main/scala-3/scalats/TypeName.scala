package scalats

/**
 * The name of a scala type
 *
 * The complete type name, including both `package`/`object` paths and type parameters, is represented by the `raw` field.
 *
 * The full type name, including `package`/`object` paths but not type parameters, is represented by the `full` field.
 *
 * The base type name, excluding both `package`/`object` paths and type parameters, is represented by the `base` field.
 *
 * For example:
 *
 * ```scala
 * val typeName = TypeName("foo.bar.Baz[Quux]")
 * typeName.raw // foo.bar.Baz[Quux]
 * typeName.full // foo.bar.Baz
 * typeName.base // Baz
 * ```
 */
class TypeName private (val raw: String) {
  final val full: String = raw.split('[').head
  final val base: String = full.split('.').last
  override final lazy val toString: String = raw
}

object TypeName {
  def apply(raw: String): TypeName = new TypeName(raw) {}
}
