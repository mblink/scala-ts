# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

BondLink's fork of scala-ts (published as `"bondlink" %% "scala-ts"`): a Scala 3 macro library that turns Scala types into TypeScript type definitions plus matching [io-ts](https://github.com/gcanti/io-ts) codecs. Its main consumer is the BondLink monorepo's `web-generate` project (`web/generate/src/main/scala/bl/gen/`), which registers types with `parse[T]` and writes `web/assets/scripts/generated/**`. The upstream-derived `README.md` is out of date (Travis, upstream docs links); trust this file and the code.

## Type safety is the governing principle

This library exists so that a Scala type and its TypeScript codec cannot disagree. Type safety is the most important constraint on both sides of that boundary — in the Scala that does the generating, and in the TypeScript it emits.

- **Make illegal states unrepresentable.** Model with sealed traits / `enum`s and `Option`/`Either`, not flag-bag records, `null` or sentinel strings. If the generator needs a new distinction, add it to `TsModel` (or a private ADT in the generator) and let exhaustive matches find every place that must handle it.
- **The compiler is the primary correctness tool.** Change the type first, then follow the errors (`cdd`, `compiler-driven-verification`).
- **Never defeat the type system.** No new `asInstanceOf`/`isInstanceOf`, no `null`, no catch-all `case _ =>` over `TsModel` or any other sealed type — list every case so a new variant breaks the build. A few wildcards over `TsModel` predate this rule (`recordCodecType`, `recordValueType`, `recordCodecInstance`, `generateTopLevel`); don't copy them, and replace one with explicit cases when you touch it. The existing `asInstanceOf`s are confined to where macro-captured runtime values arrive as `Any` (the `toList`/`toNel`/`toEither`/`toIor` closures built in `TsParser`, and `TsGenerator.generateValueInstance`); new conversions follow that pattern — capture a typed function at parse time — rather than casting elsewhere.
- **Lints are a second compiler.** `sbt-tpolecat` compiles with `-Werror`, `-Ycheck-all-patmat`, `-Wvalue-discard`, `-Wnonunit-statement` and every `-Wunused:*` flag. Zero warnings. **Never add a new `@nowarn`** to get past one; change the code. The one standing exception is the quote-pattern `@annotation.nowarn("msg=match may not be exhaustive")` on `'[t]` type matches inside macros, which the compiler cannot prove exhaustive.
- **The generated TypeScript is held to the same standard.** It must type-check under `strict`, must not resolve to `{}` or `any`, and must be readonly where the value is immutable. A change that makes generated output weaker to make the Scala simpler is the wrong trade.

## Commands

The integration tests shell out to `ts-node`, so install the TS toolchain once before testing:

```bash
cd tests && yarn && cd ..                     # install typescript/ts-node/io-ts/fp-ts for the tests
sbt Test/compile                              # type check + lint (-Werror) for main and test sources
sbt test                                      # full test suite (what CI runs, after the yarn step)
sbt 'testOnly scalats.tests.InterfaceTest'    # a single suite
sbt githubWorkflowCheck                       # CI also runs this; fails if .github/workflows is stale
sbt githubWorkflowGenerate                    # regenerate .github/workflows after changing the githubWorkflow* settings in build.sbt
```

There is no scalafmt or scalafix configuration; the compiler flags are the whole lint layer. Never hand-edit `.github/workflows/*.yml` — they are generated from `build.sbt`.

CI (`.github/workflows/ci.yml`) runs `githubWorkflowCheck`, the `yarn` install and `sbt test` on Temurin 8, 11, 17, 21 and 25, so main sources must stay Java 8 compatible.

## Architecture

Generation is two stages: a compile-time **parse** into a data model, then a run-time **render** of that model to text.

1. **`parse[A]` / `parseReference[A]`** (`package.scala`) are `inline` macros backed by `TsParser` (`TsParser.scala`). At the caller's compile time they inspect `A` with `scala.quoted` reflection and emit an `Expr[TsModel]` — structure, not TypeScript.
   - `parseTopLevel` replaces a generic type's parameters with placeholders `TypeParam["A1"]`…`TypeParam["A22"]` (rendered as `A1`, `A2`, …) and wraps a non-generic type alias in `TsModel.TypeAlias`.
   - `parse[A](top)` matches leaf types first (numbers, `String`, dates, `UUID`, collections, `Option`, `Either`/`\/`, `Ior`/`\&/`, `Map`, tuples, circe `Json`), then Scala 3 union types, then falls back to `Mirror`: sum → `Union`, product → `Interface`, singleton → `Object`. With `top = false` it produces only a reference (`UnionRef`, `InterfaceRef`, `ObjectRef`), so each type is fully parsed only where it is defined. Anything without a mirror becomes `TsModel.Unknown`, which renders as an import of a codec the consumer must supply (via `TsCustomType` or another generated file).
   - Case class fields come from the mirror; `object` fields are its public `val`s (sorted by name) with their runtime values captured, so they can be emitted as literals.
   - Sum members are tagged: a case class inside a union gets `parent = Some(...)`, and the generator adds a `_tag: t.literal("Name")` field (matching circe's `_tag` discriminator in the consumer).
2. **`TsModel`** (`TsModel.scala`) is the sealed ADT shared by both stages. Every generator function is an exhaustive match over it.
3. **`TsGenerator`** (`TsGenerator.scala`) renders a `TsModel` to `Generated` (code text + the `TsImports` it needs). For each model there are four renderers that must stay in agreement:
   - `generateCodecType` — the io-ts codec type (`t.TypeC<{...}>`)
   - `generateValueType` — the TS value type (`{ ... }`)
   - `generateCodecInstance` — the codec value (`t.type({...})`); `State.top` decides whether it is a top-level definition (passed to `state.wrapCodec`) or an inline reference
   - `generateValueInstance` — a literal TS value for a captured Scala value (used for `object` constants)

   `generateTopLevel` ties them together into the standard triple:
   ```ts
   export type FooC = t.TypeC<{ ... }>;
   export type Foo = { ... };
   export const fooC: FooC = t.type({ ... }) satisfies t.Type<Foo, unknown>;
   ```
   Generic types become codec functions (`<A1 extends t.Mixed>(A1: A1) => ...`). Unions also emit `allXC`, `allXNames`, `XName`, `XCU`/`XU`, `xOrd` (when every member is an object), `allX` and `XMap<A>`. Objects in a union use a `_tag`-only wire codec plus a `t.Type` that re-attaches the constant fields on decode. Naming: codec consts are `decap(base) + "C"` (kept as-is when the name is all caps), union codecs `base + "CU"`.
4. **`TsImports`** (`TsImports.scala`): every reference to `io-ts`, `fp-ts`, `io-ts-types` or another generated type carries its import. `TsImports.Config` sets where each comes from (and has no default for `BigNumber`, `LocalDate` and `These` — the consumer must configure them); `TsImports.Available` exposes them as `Generated`/`CallableImport` helpers. Cross-file references are `TsImport.Unresolved(typeName)` until `writeAll`/`resolve` maps each type to its output file, producing relative imports aliased `importedN_Name`. A type referenced but never generated fails here, at run time, with `UnresolvedTsImportException`.
5. **`TsCustomType` / `TsCustomOrd`** let the consumer replace generation for a type, matched by its fully-qualified name **string** — renaming or moving the Scala type silently falls back to structural generation.
6. **`TypeSorter`** orders the definitions within a file so each const/type is declared before it is used (regex scan of the generated code for names).
7. **`generateAll` → `writeAll`** (`package.scala`) is the public pipeline; `referenceCode` renders just the code needed to refer to a model.

When adding a new kind of output, the change is usually: extend `TsModel` (or reuse a case), handle it in `TsParser.parse`, then in **every** exhaustive match in `TsGenerator` — the compiler lists them.

## Tests

Each suite in `src/test/scala/scalats/tests/` extends `CodecTest`, a munit `ScalaCheckSuite` that:

1. runs `writeAll` for its types into `tests/output/<suite>/` (gitignored, deleted after the run),
2. asserts each file's content equals the expected TypeScript string in the suite's companion object — the golden test that pins exact output, and
3. writes `decode-encode.ts`, then for ScalaCheck-generated values (5 by default) runs `yarn ts-node` on it with the circe-encoded JSON, decodes and re-encodes through the generated codec, and checks the circe-decoded result round-trips. `ts-node` type-checks with `strict: true`, so generated code that does not type-check fails here.

Only files imported (transitively) by `decode-encode.ts` are type-checked; make the suite's decode-encode type the one that references everything you want checked. Test types derive `Arbitrary` via `scalats.tests.arbitrary` and circe codecs via `ConfiguredDecoder`/`ConfiguredEncoder` with the `_tag` discriminator (`tests/package.scala`). New output shapes get a new or updated suite with the full expected file text.

## Using a local checkout from BondLink

BondLink's `build.sbt` swaps the published artifact for `RootProject(file("$HOME/scala-ts"))` when sbt runs with `SCALATS_DEVELOPMENT=1`, so this checkout must be at (or symlinked from) `~/scala-ts`. Regenerate there with `yarn workspace web rungulp buildModels`. Releases are consumed by version: bump `version` in `build.sbt` and the `scalaTs` version in BondLink's `project/Dependencies.scala`.

## Code conventions

- Follow the existing style: 2-space indent, trailing commas in multi-line argument lists, `cats` syntax, string building with `|+|` on `Generated`/`imports.lift`.
- Scala 3 only (`3.3.x`); prefer `given`/`using`, `extension`, and context functions to `implicit` (the existing `implicit def liftString` predates this).
- Fail generation loudly: an unsupported shape is a `sys.error`/`report.errorAndAbort` with the type name, never a silent fallback to looser output.
- **No explanatory comments.** No new descriptive or inline "why" comments — names and types carry the meaning. The existing Scaladoc on public entry points is API documentation: keep it true when you change the behavior it describes, and don't add Scaladoc to private helpers. A comment earns a place only as a one-line, still-live constraint someone would otherwise undo.
- A comment never narrates how the code got here (no "previously", "renamed from", or bugs hit while writing it) and carries no measured figure (test counts, timings).

## Commits and pull requests

- Conventional commit subjects (`fix(generator): …`, `chore(claude): …`); a body only when the subject can't carry it — two or three bullets.
- PR description: one or two sentences plus a handful of bullets, present tense, describing the end state of the diff. No history of how you got there, no alternatives considered, and no list of the verification you ran. Keep each paragraph and bullet on a single line.
- When a change alters generated output, say so and show the before/after TypeScript, since every consumer's generated files change on upgrade.
- No AI attribution of any kind in commits, PRs, code or files.

## Skills

Detailed guidance lives in `.claude/skills/`. Read the `SKILL.md` of every skill in the matching row before your first edit — the description is an index, not the content.

| Task | Skills |
| --- | --- |
| Writing/editing Scala here | `cdd`, `scala-type-safety`, `dry` |
| Changing what TypeScript is generated | `scala-ts-generation`, `io-ts`, `cdd`, `compiler-driven-verification` |
| New feature or multi-step change | `writing-plans`, then `compiler-driven-verification` |
| Executing a plan | `executing-plans` |
| Debugging (wrong output, macro failure, test failure) | `systematic-debugging` |
| Code duplication | `dry` |
| Completing work (always, when code changed) | `verification-before-completion` |
