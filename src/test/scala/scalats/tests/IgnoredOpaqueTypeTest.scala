package scalats
package tests

import cats.Id
import io.circe.{Decoder, Encoder}
import org.scalacheck.Arbitrary

object IgnoredOpaqueTypeTestTypes {
  opaque type Foo = Int
  object Foo {
    given arb: Arbitrary[Foo] = Arbitrary.arbInt
    given decoder: Decoder[Foo] = Decoder.decodeInt
    given encoder: Encoder[Foo] = Encoder.encodeInt
  }

  inline val fooName = "scalats.tests.IgnoredOpaqueTypeTestTypes.Foo"
}

object IgnoredOpaqueTypeTest {
  val expectedFooCode = """
import * as t from "io-ts";

export type FooC = t.RefinementC<t.NumberC, number>;
export type Foo = number;
export const fooC: FooC = t.Integer satisfies t.Type<Foo, unknown>;
""".trim

  val fooFile = "foo.ts"

  inline given ignoreOpaque: TsIgnoreOpaque = TsIgnoreOpaque(Set(IgnoredOpaqueTypeTestTypes.fooName))

  val types = Map(fooFile -> (List(parse[IgnoredOpaqueTypeTestTypes.Foo]), expectedFooCode))
}

class IgnoredOpaqueTypeTest extends CodecTest[IgnoredOpaqueTypeTestTypes.Foo](
  outputDir / "ignored-opaque-type",
  IgnoredOpaqueTypeTest.types,
  "fooC",
  "fooC",
  IgnoredOpaqueTypeTest.fooFile,
) {
  override val customType: TsCustomType =
    new TsCustomType {
      def apply(name: String): Option[ReferenceCode[Id]] = name match {
        case IgnoredOpaqueTypeTestTypes.fooName =>
          Some(ReferenceCode(
            valueType = Generated.lift("number"),
            codecType = Generated(imports.iotsImport, "t.RefinementC<t.NumberC, number>"),
            codecValue = Generated(imports.iotsImport, "t.Integer"),
          ))
      }
    }
}
