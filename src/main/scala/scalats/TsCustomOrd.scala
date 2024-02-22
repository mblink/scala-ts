package scalats

/**
 * Used as an argument to [[scalats.TsGenerator]] to optionally provide custom `Ord` instances for certain types
 */
trait TsCustomOrd {
  def apply(typeName: TypeName): Option[Generated]
}

object TsCustomOrd {
  /** An instance that does not provide custom `Ord` instances for any types */
  val none = new TsCustomOrd { def apply(typeName: TypeName) = None }
}
