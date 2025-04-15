package scalats

import cats.Id

/**
 * Used as an argument to [[scalats.TsGenerator]] to optionally customize the TypeScript generation behavior
 * for certain types
 */
trait TsCustomType {
  def apply(name: String): Option[ReferenceCode[Id]]
}

object TsCustomType {
  /** An instance that does not customize generation behavior for any types */
  val none = new TsCustomType {
    def apply(name: String) = None
  }
}
