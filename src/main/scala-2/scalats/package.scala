package object scalats {
  private[scalats] def decapitalize(s: String): String = s.take(1).toLowerCase ++ s.drop(1)
  private[scalats] def unionOrdName(s: String): String = decapitalize(s) ++ "Ord"
}
