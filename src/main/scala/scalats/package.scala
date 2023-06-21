package sts

import cats.{Foldable, Monoid}

extension [F[_], A](fa: F[A])(using F: Foldable[F]) {
  final def intercalateMap[B](glue: B)(f: A => B)(implicit B: Monoid[B]): B =
    B.intercalate(glue).combineAllOption(F.toIterable(fa).map(f)).getOrElse(B.empty)
}

inline def generate[A](
  using inline customType: ScalaTs.CustomType,
  inline imports: TsImports.available,
): (List[(Option[TypeName], Generated)], ReferencedTypes) =
  ScalaTs.generate[A](customType, imports)
