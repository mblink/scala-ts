package scalats

trait TsCustomType {
  def apply(name: String): Option[Generated]
}

object TsCustomType {
  val none = new TsCustomType { def apply(name: String) = None }
}
