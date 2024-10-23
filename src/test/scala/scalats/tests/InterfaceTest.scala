package scalats
package tests

import io.circe.{Decoder, Encoder}
import scalats.tests.arbitrary.given

object InterfaceTest {
  case class Foo(int: Int, str: String)
  object Foo {
    given decoder: Decoder[Foo] = Decoder.derivedConfigured
    given encoder: Encoder[Foo] = Encoder.AsObject.derivedConfigured
  }
  case class Bar(foo: Foo, bool: Boolean)
  object Bar {
    given decoder: Decoder[Bar] = Decoder.derivedConfigured
    given encoder: Encoder[Bar] = Encoder.AsObject.derivedConfigured
  }

  val expectedFooCode = """
import * as t from "io-ts";

export type FooC = t.TypeC<{
  int: t.NumberC,
  str: t.StringC
}>;
export const fooC: FooC = t.type({
  int: t.number,
  str: t.string
});
export type Foo = t.TypeOf<FooC>;
""".trim

  val expectedBarCode = """
import { FooC as imported0_FooC, fooC as imported0_fooC } from "./foo";
import * as t from "io-ts";

export type BarC = t.TypeC<{
  foo: imported0_FooC,
  bool: t.BooleanC
}>;
export const barC: BarC = t.type({
  foo: imported0_fooC,
  bool: t.boolean
});
export type Bar = t.TypeOf<BarC>;
""".trim

  val fooFile = "foo.ts"
  val barFile = "bar.ts"

  val types = Map(
    fooFile -> (List(parse[Foo]), expectedFooCode),
    barFile -> (List(parse[Bar]), expectedBarCode),
  )
}

class InterfaceTest extends CodecTest[InterfaceTest.Bar](
  outputDir / "interface",
  InterfaceTest.types,
  "barC",
  "barC",
  InterfaceTest.barFile,
)
