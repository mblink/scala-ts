package scalats
package tests

import io.circe.derivation.{ConfiguredDecoder, ConfiguredEncoder}
import org.scalacheck.Arbitrary
import scalats.tests.arbitrary.*

object SetOfInterfaceTest {
  sealed trait Change derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder
  case object Added extends Change
  case object Removed extends Change

  case class Pos(line: Int) derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder
  case class Diff(change: Change, target: String, start: Pos, end: Option[Pos]) derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder
  case class Diffs(changes: Set[Diff]) derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder

  val expectedChangeCode = """
import * as t from "io-ts";
import { Ord as stringOrd } from "fp-ts/lib/string";
import * as E from "fp-ts/lib/Either";
import { pipe } from "fp-ts/lib/function";
import * as Ord from "fp-ts/lib/Ord";

export const added = {
  _tag: `Added`
} as const;

export type AddedTaggedC = t.TypeC<{
  _tag: t.LiteralC<`Added`>
}>;
export const addedTaggedC: AddedTaggedC = t.type({
  _tag: t.literal(`Added`)
});
export type AddedTagged = t.TypeOf<AddedTaggedC>;
export type Added = AddedTagged & typeof added;
export type AddedC = t.Type<Added, AddedTagged>;
export const addedC: AddedC = pipe(addedTaggedC, c => new t.Type<Added, AddedTagged>(
  `Added`,
  (u: unknown): u is Added => E.isRight(c.decode(u)),
  (u: unknown): E.Either<t.Errors, Added> => pipe(c.decode(u), E.map(x => ({ ...x, ...added }))),
  (x: Added): AddedTagged => ({ ...x, _tag: `Added`}),
)) satisfies t.Type<Added, unknown>;


export const removed = {
  _tag: `Removed`
} as const;

export type RemovedTaggedC = t.TypeC<{
  _tag: t.LiteralC<`Removed`>
}>;
export const removedTaggedC: RemovedTaggedC = t.type({
  _tag: t.literal(`Removed`)
});
export type RemovedTagged = t.TypeOf<RemovedTaggedC>;
export type Removed = RemovedTagged & typeof removed;
export type RemovedC = t.Type<Removed, RemovedTagged>;
export const removedC: RemovedC = pipe(removedTaggedC, c => new t.Type<Removed, RemovedTagged>(
  `Removed`,
  (u: unknown): u is Removed => E.isRight(c.decode(u)),
  (u: unknown): E.Either<t.Errors, Removed> => pipe(c.decode(u), E.map(x => ({ ...x, ...removed }))),
  (x: Removed): RemovedTagged => ({ ...x, _tag: `Removed`}),
)) satisfies t.Type<Removed, unknown>;


export const allChangeC = [addedC, removedC] as const;
export const allChangeNames = [`Added`, `Removed`] as const;
export type ChangeName = (typeof allChangeNames)[number];

export type ChangeCU = t.UnionC<[AddedC, RemovedC]>;
export type ChangeU = Added | Removed;
export const ChangeCU: ChangeCU = t.union([addedC, removedC]) satisfies t.Type<ChangeU, unknown>;

export const changeOrd: Ord.Ord<ChangeU> = pipe(stringOrd, Ord.contramap(x => x._tag));
export const allChange = [added, removed] as const;
export type ChangeMap<A> = { [K in ChangeName]: A };
""".trim

  val expectedDiffCode = """
import * as t from "io-ts";
import { Ord as stringOrd } from "fp-ts/lib/string";
import * as O from "fp-ts/lib/Option";
import { pipe } from "fp-ts/lib/function";
import * as Ord from "fp-ts/lib/Ord";
import { OptionFromNullableC, optionFromNullable } from "io-ts-types/lib/optionFromNullable";
import { ChangeCU as imported1_ChangeCU, ChangeU as imported0_ChangeU, ChangeCU as imported0_ChangeCU, changeOrd as imported0_changeOrd } from "./change";
import { Ord as numberOrd } from "fp-ts/lib/number";

export type PosC = t.TypeC<{
  line: t.NumberC
}>;
export type Pos = {
  line: number
};
export const posC: PosC = t.type({
  line: t.number
}) satisfies t.Type<Pos, unknown>;
export const posOrd: Ord.Ord<Pos> = pipe(Ord.tuple(numberOrd), Ord.contramap((x: Pos) => [x.line] as const));


export type DiffC = t.TypeC<{
  change: imported1_ChangeCU,
  target: t.StringC,
  start: PosC,
  end: OptionFromNullableC<PosC>
}>;
export type Diff = {
  change: imported0_ChangeU,
  target: string,
  start: Pos,
  end: O.Option<Pos>
};
export const diffC: DiffC = t.type({
  change: imported0_ChangeCU,
  target: t.string,
  start: posC,
  end: optionFromNullable(posC)
}) satisfies t.Type<Diff, unknown>;
export const diffOrd: Ord.Ord<Diff> = pipe(Ord.tuple(imported0_changeOrd, stringOrd, posOrd, O.getOrd(posOrd)), Ord.contramap((x: Diff) => [x.change, x.target, x.start, x.end] as const));
""".trim

  val expectedDiffsCode = """
import { ReadonlySetFromArrayC, readonlySetFromArray } from "io-ts-types/lib/readonlySetFromArray";
import { DiffC as imported0_DiffC, Diff as imported0_Diff, diffC as imported0_diffC, diffOrd as imported0_diffOrd } from "./diff";
import * as t from "io-ts";

export type DiffsC = t.TypeC<{
  changes: ReadonlySetFromArrayC<imported0_DiffC>
}>;
export type Diffs = {
  changes: ReadonlySet<imported0_Diff>
};
export const diffsC: DiffsC = t.type({
  changes: readonlySetFromArray(imported0_diffC, imported0_diffOrd)
}) satisfies t.Type<Diffs, unknown>;
""".trim

  val changeFile = "change.ts"
  val diffFile = "diff.ts"
  val diffsFile = "diffs.ts"

  val types = Map(
    changeFile -> (List(parse[Change]), expectedChangeCode),
    diffFile -> (List(parse[Pos], parse[Diff]), expectedDiffCode),
    diffsFile -> (List(parse[Diffs]), expectedDiffsCode),
  )
}

class SetOfInterfaceTest extends CodecTest[SetOfInterfaceTest.Diffs](
  outputDir / "setOfInterface",
  SetOfInterfaceTest.types,
  "diffsC",
  "diffsC",
  SetOfInterfaceTest.diffsFile,
)

object InterfaceOrdTest {
  case class Key(id: Int, name: String)
  case class ByKey(values: Map[Key, String])

  case class WithUuid(id: java.util.UUID)
  case class WithUuids(ids: Set[WithUuid])

  case class SetTree(label: String, children: Set[SetTree])

  case class ListTree(label: String, children: List[ListTree])
  case class ListForest(trees: Set[ListTree])

  case class Left(rights: Set[Right])
  case class Right(lefts: Set[Left])

  case class Custom(value: String)
  case class Customs(customs: Set[Custom])

  val expectedByKeyCode = """
import * as t from "io-ts";
import { Ord as stringOrd } from "fp-ts/lib/string";
import { pipe } from "fp-ts/lib/function";
import * as Ord from "fp-ts/lib/Ord";
import { ReadonlyMapFromEntriesC, readonlyMapFromEntries } from "io-ts-types/lib/readonlyMapFromEntries";
import { Ord as numberOrd } from "fp-ts/lib/number";

export type KeyC = t.TypeC<{
  id: t.NumberC,
  name: t.StringC
}>;
export type Key = {
  id: number,
  name: string
};
export const keyC: KeyC = t.type({
  id: t.number,
  name: t.string
}) satisfies t.Type<Key, unknown>;
export const keyOrd: Ord.Ord<Key> = pipe(Ord.tuple(numberOrd, stringOrd), Ord.contramap((x: Key) => [x.id, x.name] as const));


export type ByKeyC = t.TypeC<{
  values: ReadonlyMapFromEntriesC<KeyC, t.StringC>
}>;
export type ByKey = {
  values: ReadonlyMap<Key, string>
};
export const byKeyC: ByKeyC = t.type({
  values: readonlyMapFromEntries(keyC, keyOrd, t.string)
}) satisfies t.Type<ByKey, unknown>;
""".trim
}

class InterfaceOrdTest extends munit.FunSuite {
  import InterfaceOrdTest.*

  private val file = outputDir / "interfaceOrd" / "types.ts"

  private def generate(models: List[TsModel])(using TsCustomType): String = {
    writeAll(Map(file -> models))
    val code = file.read.trim
    file.delete
    code
  }

  private def ordError(models: List[TsModel])(using TsCustomType): String =
    intercept[RuntimeException](generate(models)).getMessage

  test("a case class used as a Map key gets a named Ord") {
    assertEquals(generate(List(parse[Key], parse[ByKey])), expectedByKeyCode)
  }

  test("a case class field without an Ord fails generation") {
    assertEquals(
      ordError(List(parse[WithUuid], parse[WithUuids])),
      "`Ord` instance requested for java.util.UUID but not found",
    )
  }

  test("a case class that contains a Set of itself fails generation") {
    assertEquals(
      ordError(List(parse[SetTree])),
      "`Ord` instance requested for scala.collection.immutable.Set but not found",
    )
  }

  test("a recursive case class in a Set fails generation") {
    assertEquals(
      ordError(List(parse[ListTree], parse[ListForest])),
      "`Ord` instance requested for scalats.tests.InterfaceOrdTest.ListTree but not found",
    )
  }

  test("mutually recursive case classes through Sets fail generation") {
    assertEquals(
      ordError(List(parse[Left], parse[Right])),
      "`Ord` instance requested for scala.collection.immutable.Set but not found",
    )
  }

  test("a custom type without a custom Ord fails generation") {
    val customC = ReferenceCode[Option](
      codecType = imports.lift("CustomC"),
      codecInstance = imports.lift("customC"),
      valueType = None,
      valueInstance = None,
    )
    given TsCustomType = new TsCustomType {
      def apply(name: String): Option[ReferenceCode[Option]] =
        Option.when(name == "scalats.tests.InterfaceOrdTest.Custom")(customC)
    }
    assertEquals(
      ordError(List(parse[Customs])),
      "`Ord` instance requested for scalats.tests.InterfaceOrdTest.Custom but not found",
    )
  }
}
