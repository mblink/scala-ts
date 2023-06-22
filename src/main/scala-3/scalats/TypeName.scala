package scalats

class TypeName private (val raw: String) {
  final val full: String = raw.split('[').head
  final val base: String = full.split('.').last
  override final lazy val toString: String = raw
}

object TypeName {
  def apply(raw: String): TypeName = new TypeName(raw) {}
}
