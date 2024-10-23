package scalats

/**
 * Used as an argument to [[scalats.TsGenerator]] to optionally customize the TypeScript generation behavior
 * for certain types
 */
trait TsCustomType {
  def tpe(name: String): Option[Generated]
  def value(name: String): Option[Generated]
}

object TsCustomType {
  /** An instance that does not customize generation behavior for any types */
  val none = new TsCustomType {
    def tpe(name: String) = None
    def value(name: String) = None
  }
}
