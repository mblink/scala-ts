# scala-ts

A Scala 3 macro library that generates TypeScript types and [`io-ts`](https://github.com/gcanti/io-ts) codecs from Scala types. The Scala types are the source of truth for an API's I/O types; the generated TypeScript hardens the contract between the frontend and the backend.

## Installation

```scala
// build.sbt
resolvers += "bondlink-maven-repo" at "https://maven.bondlink-cdn.com"
libraryDependencies += "bondlink" %% "scala-ts" % "0.21.0"
```

Requires Scala 3 (currently built against 3.3.7).

## Quickstart

The recommended setup is to define your types in one sbt subproject and your generator in another. The generator project depends on the types project and runs as a normal `main` method, regenerating TypeScript files on demand.

1. Define your types:

    ```scala
    // build.sbt
    lazy val types = project.in(file("types"))

    // types/src/main/scala/com/example/Role.scala
    package com.example

    enum Role { case User, Admin, SuperAdmin }

    // types/src/main/scala/com/example/User.scala
    package com.example

    case class User(name: String, email: String, role: Role)
    ```

2. Define a generator project that depends on `types` and on `scala-ts`:

    ```scala
    // build.sbt
    lazy val generate = project.in(file("generate"))
      .dependsOn(types)
      .settings(
        resolvers += "bondlink-maven-repo" at "https://maven.bondlink-cdn.com",
        libraryDependencies += "bondlink" %% "scala-ts" % "0.21.0",
      )

    // generate/src/main/scala/com/example/Generate.scala
    package com.example

    import java.io.File
    import scalats.{parse, TsCustomOrd, TsCustomType, TsImports, writeAll}

    object Generate {
      given customOrd: TsCustomOrd = TsCustomOrd.none
      given customType: TsCustomType = TsCustomType.none
      given imports: TsImports.Available = TsImports.Available(TsImports.Config())

      def main(args: Array[String]): Unit =
        writeAll(Map(
          new File("/path/to/your/repo/generated/role.ts") -> List(parse[Role]),
          new File("/path/to/your/repo/generated/user.ts") -> List(parse[User]),
        ))
    }
    ```

3. Regenerate TypeScript whenever the Scala types change:

    ```bash
    sbt generate/run
    ```

## Output

The output generated from the [quickstart](#quickstart) example looks like this:

<details><summary>expand <code>generated/role.ts</code></summary>

```ts
import * as t from "io-ts";
import { Ord as stringOrd } from "fp-ts/lib/string";
import * as E from "fp-ts/lib/Either";
import { pipe } from "fp-ts/lib/function";
import * as Ord from "fp-ts/lib/Ord";

export const user = {
  _tag: `User`
} as const;

export type UserTaggedC = t.TypeC<{
  _tag: t.LiteralC<`User`>
}>;
export const userTaggedC: UserTaggedC = t.type({
  _tag: t.literal(`User`)
});
export type UserTagged = t.TypeOf<UserTaggedC>;
export type User = UserTagged & typeof user;
export type UserC = t.Type<User, UserTagged>;
export const userC: UserC = pipe(userTaggedC, c => new t.Type<User, UserTagged>(
  `User`,
  (u: unknown): u is User => E.isRight(c.decode(u)),
  (u: unknown): E.Either<t.Errors, User> => pipe(c.decode(u), E.map(x => ({ ...x, ...user }))),
  (x: User): UserTagged => ({ ...x, _tag: `User`}),
)) satisfies t.Type<User, unknown>;


export const admin = {
  _tag: `Admin`
} as const;

export type AdminTaggedC = t.TypeC<{
  _tag: t.LiteralC<`Admin`>
}>;
export const adminTaggedC: AdminTaggedC = t.type({
  _tag: t.literal(`Admin`)
});
export type AdminTagged = t.TypeOf<AdminTaggedC>;
export type Admin = AdminTagged & typeof admin;
export type AdminC = t.Type<Admin, AdminTagged>;
export const adminC: AdminC = pipe(adminTaggedC, c => new t.Type<Admin, AdminTagged>(
  `Admin`,
  (u: unknown): u is Admin => E.isRight(c.decode(u)),
  (u: unknown): E.Either<t.Errors, Admin> => pipe(c.decode(u), E.map(x => ({ ...x, ...admin }))),
  (x: Admin): AdminTagged => ({ ...x, _tag: `Admin`}),
)) satisfies t.Type<Admin, unknown>;


export const superAdmin = {
  _tag: `SuperAdmin`
} as const;

export type SuperAdminTaggedC = t.TypeC<{
  _tag: t.LiteralC<`SuperAdmin`>
}>;
export const superAdminTaggedC: SuperAdminTaggedC = t.type({
  _tag: t.literal(`SuperAdmin`)
});
export type SuperAdminTagged = t.TypeOf<SuperAdminTaggedC>;
export type SuperAdmin = SuperAdminTagged & typeof superAdmin;
export type SuperAdminC = t.Type<SuperAdmin, SuperAdminTagged>;
export const superAdminC: SuperAdminC = pipe(superAdminTaggedC, c => new t.Type<SuperAdmin, SuperAdminTagged>(
  `SuperAdmin`,
  (u: unknown): u is SuperAdmin => E.isRight(c.decode(u)),
  (u: unknown): E.Either<t.Errors, SuperAdmin> => pipe(c.decode(u), E.map(x => ({ ...x, ...superAdmin }))),
  (x: SuperAdmin): SuperAdminTagged => ({ ...x, _tag: `SuperAdmin`}),
)) satisfies t.Type<SuperAdmin, unknown>;


export const allRoleC = [userC, adminC, superAdminC] as const;
export const allRoleNames = [`User`, `Admin`, `SuperAdmin`] as const;
export type RoleName = (typeof allRoleNames)[number];

export type RoleCU = t.UnionC<[UserC, AdminC, SuperAdminC]>;
export type RoleU = User | Admin | SuperAdmin;
export const RoleCU: RoleCU = t.union([userC, adminC, superAdminC]) satisfies t.Type<RoleU, unknown>;

export const roleOrd: Ord.Ord<RoleU> = pipe(stringOrd, Ord.contramap(x => x._tag));
export const allRole = [user, admin, superAdmin] as const;
export type RoleMap<A> = { [K in RoleName]: A };
```

</details>

<details><summary>expand <code>generated/user.ts</code>

</summary>

```ts
import * as t from "io-ts";
import { RoleCU as imported1_RoleCU, RoleU as imported0_RoleU, RoleCU as imported0_RoleCU } from "./role";

export type UserC = t.TypeC<{
  name: t.StringC,
  email: t.StringC,
  role: imported1_RoleCU
}>;
export type User = {
  name: string,
  email: string,
  role: imported0_RoleU
};
export const userC: UserC = t.type({
  name: t.string,
  email: t.string,
  role: imported0_RoleCU
}) satisfies t.Type<User, unknown>;
```

</details>

## Configuration

`generateAll`, `writeAll`, and `referenceCode` all require three given values. Default instances are provided but must be opted into explicitly:

| Given                    | Default                              | Purpose                                                                 |
| ------------------------ | ------------------------------------ | ----------------------------------------------------------------------- |
| `TsCustomType`           | `TsCustomType.none`                  | Override the generated TS for specific Scala types.                     |
| `TsCustomOrd`            | `TsCustomOrd.none`                   | Provide `fp-ts` `Ord` instances for types used in `Set`s.               |
| `TsImports.Available`    | `TsImports.Available(TsImports.Config())` | Controls where `fp-ts` / `io-ts` values are imported from.         |

### Customizing import locations

`TsImports.Config` exposes the import path used for every `fp-ts` and `io-ts` value the generator emits. Every field has a sane default; override individual fields to point at hand-written codecs or alternate libraries. A few fields are `Option` and have no default; they only matter if the corresponding Scala types appear in your model:

- `iotsLocalDate` — required to support `java.time.LocalDate` and `org.joda.time.LocalDate`
- `iotsBigNumber` — required to support arbitrary-precision number types
- `iotsThese` — required to support `cats.data.Ior` and `scalaz.\&/`

### Customizing types via `TsCustomType`

`TsCustomType` lets you map a Scala type name to hand-written TypeScript. Use it when you have a Scala type whose default representation is not what you want, or whose representation is not built in. This is the intended extension point for adding support for additional types.

For example, you might have a `Money` type in Scala for which you'd like to handwrite an `io-ts` codec. To handle it, you would define your `TsCustomType` instance to point to the TypeScript codec and type:

```scala
val moneyPath = "/path/to/custom/codecs/money.ts"

given customType: TsCustomType = new TsCustomType {
  def apply(name: String): Option[ReferenceCode[Option]] = name match {
    case "com.example.Money" =>
      Some(ReferenceCode(
        // Assumes `given imports: TsImports.Available` is in scope
        codecType = imports.namedImport(moneyPath, "MoneyC"),
        codecInstance = imports.namedImport(moneyPath, "moneyC"),
        valueType = Some(imports.namedImport(moneyPath, "Money")),
        valueInstance = None,
      ))
    case _ => None
  }
}
```

### Providing `Ord` instances via `TsCustomOrd`

A Scala `Set[A]` is generated as `ReadonlySet<A>`, which from `fp-ts` requires an `Ord<A>`. Built-in `Ord` instances cover the primitive types; for any other element type that appears in a `Set`, supply an `Ord` via `TsCustomOrd`:

```scala
given customOrd: TsCustomOrd = new TsCustomOrd {
  def apply(typeName: TypeName): Option[Generated] = typeName.full match {
    case "java.time.LocalDate" =>
      // Assumes `given imports: TsImports.Available` is in scope
      Some(imports.namedImport("/path/to/localDateOrd.ts", "localDateOrd"))
    case _ => None
  }
}
```

`Ord` instances are only required for types that are used within `Set` elements.

## Optional dependencies

`scala-ts` declares `circe`, `joda-time`, and `scalaz` as `Optional` dependencies. None of them are required, but if any are on the user's classpath the generator picks up additional type support automatically.

### circe

- `io.circe.Json` — represented in TypeScript as `unknown`

### joda-time

- `org.joda.time.LocalDate` — requires `iotsLocalDate` to be set on `TsImports.Config` (for example, pointing at `js-joda`'s `LocalDate`)
- `org.joda.time.DateTime` — represented by default as `Date`, decoded with `io-ts-types/lib/DateFromISOString`. Override `iotsDateTime` on `TsImports.Config` to substitute another type, e.g. `js-joda`'s `ZonedDateTime`

### scalaz

- `scalaz.NonEmptyList` — `ReadonlyNonEmptyArray` from `fp-ts`
- `scalaz.\&/` — `These` from `fp-ts` (requires `iotsThese` to be configured)
- `scalaz.\/` — `Either` from `fp-ts`

## Codec target

The generated codecs are hardcoded to `io-ts`. The codec layer could in principle be made pluggable, but that is not currently planned.

## Public API

All public entry points live in the `scalats` package.

### Macros

- `parse[A]` — parse a top-level Scala type `A` into a `TsModel` for definition. Use this for types you want to *generate* in TypeScript.
- `parseReference[A]` — parse `A` as a reference only. Use this when you want to refer to `A` from generated code without redefining it.

### Generation

- `generateAll(all, debug, debugFilter)` — turn a `Map[File, List[TsModel]]` into an intermediate IR that can be fed to `writeAll` or `resolve`
- `writeAll(all)` — write the generated IR to disk
- `writeAll(all, debug, debugFilter)` — convenience overload that runs `generateAll` and writes the result in one call. This is what the [quickstart](#quickstart) uses
- `resolve(currFile, generated, allGenerated)` — resolve the imports of a single piece of generated code in the context of a larger generation pass. Useful when assembling output yourself
- `referenceCode(model)` — produce the snippets needed to refer to a parsed type from hand-written generator code (codec type, codec instance, value type, value instance)

## Code structure

The library is small enough that a one-paragraph map is enough to find your way around `src/main/scala/scalats`:

- `package.scala` — public API: `parse`, `parseReference`, `generateAll`, `writeAll`, `resolve`, `referenceCode`
- `TsParser.scala` — the macro that walks a Scala type at compile time and produces a `TsModel`
- `TsModel.scala` — the intermediate representation: every shape the parser can recognize (primitives, collections, interfaces, objects, unions, opaque types, type aliases, etc.). Interfaces, objects, and unions have both a definition form and a reference form so referenced types are not re-parsed
- `TsGenerator.scala` — turns a `TsModel` into TypeScript types and `io-ts` codecs. The bulk of the rendering logic lives here
- `TsImports.scala` — the import bookkeeping system, including `TsImports.Config` (where every `fp-ts` / `io-ts` value is imported from) and `TsImports.Available` (the live set of imports during generation). Imports can be unresolved at write time and are resolved against the full set of generated files
- `TsCustomType.scala` / `TsCustomOrd.scala` — the user-facing extension points
- `TypeSorter.scala` — topologically sorts generated types so that dependencies appear before dependents within a file
- `TypeName.scala`, `TypeParam.scala`, `ReferenceCode.scala`, `Generated.scala` — small supporting types
- `ReflectionUtils.scala` — macro helpers for `TsParser`

## License

MIT. See [LICENCE.txt](./LICENCE.txt).
