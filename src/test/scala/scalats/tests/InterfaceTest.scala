package scalats
package tests

import io.circe.derivation.{ConfiguredDecoder, ConfiguredEncoder}
import org.scalacheck.Arbitrary
import scalats.tests.arbitrary.*

object InterfaceTest {
  case class Foo(int: Int, str: String) derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder
  case class Bar(foo: Foo, bool: Boolean) derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder

  val expectedFooCode = """
import * as t from "io-ts";

export type FooC = t.TypeC<{
  int: t.NumberC,
  str: t.StringC
}>;
export type Foo = {
  int: number,
  str: string
};
export const fooC: FooC = t.type({
  int: t.number,
  str: t.string
}) satisfies t.Type<Foo, unknown>;
""".trim

  val expectedBarCode = """
import { FooC as imported0_FooC, Foo as imported0_Foo, fooC as imported0_fooC } from "./foo";
import * as t from "io-ts";

export type BarC = t.TypeC<{
  foo: imported0_FooC,
  bool: t.BooleanC
}>;
export type Bar = {
  foo: imported0_Foo,
  bool: boolean
};
export const barC: BarC = t.type({
  foo: imported0_fooC,
  bool: t.boolean
}) satisfies t.Type<Bar, unknown>;
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
