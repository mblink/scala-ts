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

export const fooC = t.type({
  int: t.number,
  str: t.string
});
export type FooC = typeof fooC;
export type Foo = t.TypeOf<FooC>;
""".trim

  val expectedBarCode = """
import { fooC as imported0_fooC } from "./foo";
import * as t from "io-ts";

export const barC = t.type({
  foo: imported0_fooC,
  bool: t.boolean
});
export type BarC = typeof barC;
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
