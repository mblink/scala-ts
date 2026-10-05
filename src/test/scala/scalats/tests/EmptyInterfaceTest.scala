package scalats
package tests

import io.circe.derivation.{ConfiguredDecoder, ConfiguredEncoder}
import org.scalacheck.Arbitrary
import scalats.tests.arbitrary.*

object EmptyInterfaceTest {
  case class Empty() derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder
  case class HasEmpty(empty: Empty, int: Int) derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder

  val expectedEmptyCode = """
import * as t from "io-ts";

export type EmptyC = t.ReadonlyC<t.UnknownRecordC>;
export type Empty = Readonly<Record<string, unknown>>;
export const emptyC: EmptyC = t.readonly(t.UnknownRecord) satisfies t.Type<Empty, unknown>;
""".trim

  val expectedHasEmptyCode = """
import { EmptyC as imported0_EmptyC, Empty as imported0_Empty, emptyC as imported0_emptyC } from "./empty";
import * as t from "io-ts";

export type HasEmptyC = t.TypeC<{
  empty: imported0_EmptyC,
  int: t.NumberC
}>;
export type HasEmpty = {
  empty: imported0_Empty,
  int: number
};
export const hasEmptyC: HasEmptyC = t.type({
  empty: imported0_emptyC,
  int: t.number
}) satisfies t.Type<HasEmpty, unknown>;
""".trim

  val emptyFile = "empty.ts"
  val hasEmptyFile = "hasEmpty.ts"

  val types = Map(
    emptyFile -> (List(parse[Empty]), expectedEmptyCode),
    hasEmptyFile -> (List(parse[HasEmpty]), expectedHasEmptyCode),
  )
}

class EmptyInterfaceTest extends CodecTest[EmptyInterfaceTest.HasEmpty](
  outputDir / "emptyInterface",
  EmptyInterfaceTest.types,
  "hasEmptyC",
  "hasEmptyC",
  EmptyInterfaceTest.hasEmptyFile,
)
