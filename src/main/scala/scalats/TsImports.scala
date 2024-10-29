package scalats

import cats.{Id, Monoid, Semigroup, Show}
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import java.io.File
import java.nio.file.Paths

/**
 * A single TypeScript import without a location specified yet.
 *
 * The import either imports everything (`scalats.TsImport_*`) or a specific set of values (`scalats.TsImportNames`)
 */
sealed trait TsImport {
  val str: String
  override final lazy val toString: String = str
}

object TsImport {
  /** The location that a [[scalats.TsImport]] comes from */
  sealed trait Location
  /** A resolved import location, i.e. a known file or package */
  case class Resolved(location: String) extends Location
  /**
   * An unresolved import location
   *
   * The only info we have about the location is that it will be the file where the given `typeName` has been generated.
   * Unresolved imports are eventually resolved based on the full set of types being generated.
   *
   * @see [[scalats.TsImports.resolve]]
   * @see [[scalats.resolve]]
   * @see [[scalats.writeAll]]
   */
  case class Unresolved(typeName: TypeName) extends Location

  given semigroup: Semigroup[TsImport] = Semigroup.instance((x, y) => (x, y) match {
    case (i @ TsImport_*(_), TsImport_*(_)) => i
    case (TsImportNames(ns), TsImportNames(ns2)) => TsImportNames(ns ++ ns2)
    case (TsImport_*(_), TsImportNames(ns)) => sys.error(s"Can't import `*` and `${ns.mkString}`")
    case (TsImportNames(ns), TsImport_*(_)) => sys.error(s"Can't import `${ns.mkString}` and `*`")
  })

  given show: Show[TsImport] = Show.show(_.str)
}

/** A TypeScript import that imports all values, i.e. `import * as $alias` */
case class TsImport_*(alias: String) extends TsImport {
  val str: String = s"* as $alias"
}

/** A TypeScript import that imports certain values, i.e. `import { ...$names }` */
case class TsImportNames(names: Set[String]) extends TsImport {
  val str: String = names.mkString("{ ", ", ", " }")
}
object TsImportNames {
  def apply(ns: String*): TsImportNames = new TsImportNames(ns.toSet)
}

/** An exception thrown when a TypeScript import can't be resolved */
class UnresolvedTsImportException(val currFile: String, val importedTypeName: TypeName)
extends Exception(s"Failed to resolve import for type `${importedTypeName.full}` from file `$currFile`")

/** A set of TypeScript imports, represented as a map of [[scalats.TsImport.Location]] to [[scalats.TsImport]] */
class TsImports private (private val m: Map[TsImport.Location, TsImport]) {
  import TsImports.*

  final lazy val toList: List[(TsImport.Location, TsImport)] = m.toList

  override final lazy val toString: String =
    s"TsImports(${toList.flatMap {
      case (TsImport.Resolved(l), i) => Some(s"""import $i from "$l"""")
      case (TsImport.Unresolved(_), i) => Some(s"""import $i from <unresolved>""")
    }.mkString(", ")})"

  /** Combine another set of [[scalats.TsImports]] with this set */
  final def ++(other: TsImports): TsImports = new TsImports(m |+| other.m)

  /**
   * Resolve this set of imports
   *
   * @param currFile The file currently being generated
   * @param allTypes All generated files and types
   * @param code The code that contains references to the values being imported
   * @return A set of resolved imports and code
   * @throws scalats.UnresolvedTsImportException
   */
  final def resolve(
    currFile: String,
    allTypes: Map[String, Set[TypeName]],
    code: String,
  ): (Map[TsImport.Resolved, TsImport], String) = {
    val currFileParent = new File(currFile).getParent
    val typeToFile = allTypes.flatMap { case (file, typeNames) => typeNames.map(t => (t.full, file)) }
    m.foldLeft((Map.empty[TsImport.Resolved, TsImport], code)) {
      case ((accImports, accCode), (TsImport.Resolved(loc), i)) =>
        loc match {
          case file @ tsFileRx(_) =>
            (accImports |+| Map(TsImport.Resolved(normalizePath(relPath(file, currFileParent))) -> i), accCode)

          case _ =>
            (accImports |+| Map(TsImport.Resolved(loc) -> i), accCode)
        }

      case ((accImports, accCode), (TsImport.Unresolved(t), i)) =>
        val file = typeToFile.getOrElse(t.full, throw new UnresolvedTsImportException(currFile, t))

        if (file == currFile) {
          (accImports, i match {
            case TsImportNames(names) =>
              names.foldLeft(accCode) {
                case (acc, importedNameRx(newName, oldName, _)) => acc.replace(oldName, newName)
                case (acc, _) => acc
              }
            case TsImport_*(_) => accCode
          })
        } else {
          (accImports |+| Map(TsImport.Resolved(normalizePath(relPath(file, currFileParent))) -> i), accCode)
        }
    }
  }
}

object TsImports {
  /** An empty set of imports */
  val empty: TsImports = new TsImports(Map())

  /**
   * A constructor to import all values from a known location, i.e. `import * as $alias from "$loc"`
   *
   * @param loc The location to import the values from
   * @param alias The alias to use for the imported values
   */
  def all(loc: String, alias: String): TsImports =
    new TsImports(Map(TsImport.Resolved(loc) -> TsImport_*(alias)))

  /**
   * A constructor to import all values from the file in which `typeName` is generated
   *
   * @param typeName The name of the type used to resolve the import
   * @param alias The alias to use for the imported values
   */
  def all(typeName: TypeName, alias: String): TsImports =
    new TsImports(Map(TsImport.Unresolved(typeName) -> TsImport_*(alias)))

  /**
   * A constructor to import certain values from a known location, i.e. `import { $name1, ...$otherNames } from "$loc"`
   *
   * @param loc The location to import the values from
   * @param name1 The name of the first value to import
   * @param otherNames More names to import
   */
  def names(loc: String, name1: String, otherNames: String*): TsImports =
    new TsImports(Map(TsImport.Resolved(loc) -> TsImportNames(otherNames.toSet + name1)))

  /**
   * A constructor to import certain values from the file in which `typeName` is generated
   *
   * @param typeName The name of the type used to resolve the import
   * @param name1 The name of the first value to import
   * @param otherNames More names to import
   */
  def names(typeName: TypeName, name1: String, otherNames: String*): TsImports =
    new TsImports(Map(TsImport.Unresolved(typeName) -> TsImportNames(otherNames.toSet + name1)))

  /**
   * Produce a [[scalats.Generated]] that refers to a value imported from a known location
   *
   * @param loc The location to import the value from
   * @param valueName The name of the value to import
   * @param alias An optional alias for the imported value
   */
  def namedImport(loc: String, valueName: String, alias: Option[String]): Generated =
    Generated(names(loc, alias.fold(valueName)(a => s"$valueName as $a")), alias.getOrElse(valueName))

  /**
   * Produce a [[scalats.Generated]] that refers to a value imported from a known location
   *
   * @param loc The location to import the value from
   * @param valueName The name of the value to import
   */
  def namedImport(loc: String, valueName: String): Generated = namedImport(loc, valueName, None)

  /**
   * Produce a [[scalats.Generated]] that refers to a value imported from the file in which `typeName` is generated
   *
   * @param typeName The name of the type used to resolve the import
   * @param valueName The name of the value to import
   * @param alias An optional alias for the imported value
   */
  def namedImport(typeName: TypeName, valueName: String, alias: Option[String]): Generated =
    Generated(names(typeName, alias.fold(valueName)(a => s"$valueName as $a")), alias.getOrElse(valueName))

  given monoid: Monoid[TsImports] = Monoid.instance(empty, _ ++ _)

  val importedNameRx = "^(.*?) as (imported(\\d+)_\\1)$".r
  private val tsFileRx = "^(.*)\\.tsx?$".r

  /** Relativize `filePath` from `fromPath` */
  private def relPath(filePath: String, fromPath: String): String =
    Paths.get(fromPath).relativize(Paths.get(filePath)).toString

  /** Normalize a file `path` for use in a TypeScript import by removing `.ts`/`.tsx` from the path */
  private def normalizePath(path: String): String = {
    val woExt = tsFileRx.replaceAllIn(path, "$1")
    if (woExt.startsWith(".")) woExt else s"./$woExt"
  }

  /** An imported TypeScript value that can be called, either as a function with parameters, or as a type with arguments */
  case class CallableImport(tsImports: TsImports, private val name: String, prefix: String = "(", suffix: String = ")") {
    lazy val value: Generated = Generated(tsImports, name)

    /** Call the import with a set of `String` arguments */
    def apply(s: String*): Generated =
      Generated(tsImports, name ++ prefix ++ s.mkString(", ") ++ suffix)

    /** Call the import with a set of [[scalats.Generated]] arguments */
    def apply(t: Generated*)(using d: DummyImplicit): Generated = {
      val w = Generated.lift(name ++ prefix) |+| t.intercalate(Generated.lift(", ")) |+| Generated.lift(suffix)
      Generated(tsImports ++ w.imports, w.code)
    }
  }

  object CallableImport {
    def apply(t: Generated): CallableImport = CallableImport(t.imports, t.code)
    def apply(t: Generated, p: String, s: String): CallableImport = CallableImport(t.imports, t.code, p, s)

    def tpe(t: Generated): CallableImport = CallableImport(t.imports, t.code, "<", ">")
    def tpe(tsImports: TsImports, name: String): CallableImport = CallableImport(tsImports, name, "<", ">")
  }

  type Never[A] = None.type

  /**
   * TypeScript imports config to control where certain values and types are imported from
   *
   * Most values have sane defaults from `fp-ts` or `io-ts` but can be overridden.
   *
   * Some `io-ts` values have no defaults but can be configured to point to hand-written codecs.
   */
  case class Config(
    fptsBoolean: String = "fp-ts/lib/boolean",
    fptsEither: String = "fp-ts/lib/Either",
    fptsNumber: String = "fp-ts/lib/number",
    fptsOption: String = "fp-ts/lib/Option",
    fptsOrd: String = "fp-ts/lib/Ord",
    fptsReadonlyArray: String = "fp-ts/lib/ReadonlyArray",
    fptsReadonlyNonEmptyArray: String = "fp-ts/lib/ReadonlyNonEmptyArray",
    fptsReadonlySet: String = "fp-ts/lib/ReadonlySet",
    fptsString: String = "fp-ts/lib/string",
    fptsThese: String = "fp-ts/lib/These",
    fptsPipe: (String, String) = ("pipe", "fp-ts/lib/function"),
    iots: String = "io-ts",
    iotsDateTime: ReferenceCode[Id] = ReferenceCode(
      valueType = Generated.lift("Date"),
      codecType = namedImport("io-ts-types/lib/DateFromISOString", "DateFromISOStringC"),
      codecValue = namedImport("io-ts-types/lib/DateFromISOString", "DateFromISOString"),
    ),
    iotsReadonlyMapFromEntries: ReferenceCode[Never] = ReferenceCode(
      valueType = None,
      codecType = namedImport("io-ts-types/lib/readonlyMapFromEntries", "ReadonlyMapFromEntriesC"),
      codecValue = namedImport("io-ts-types/lib/readonlyMapFromEntries", "readonlyMapFromEntries"),
    ),
    iotsReadonlyNonEmptyArray: ReferenceCode[Never] = ReferenceCode(
      valueType = None,
      codecType = namedImport("io-ts-types/lib/readonlyNonEmptyArray", "ReadonlyNonEmptyArrayC"),
      codecValue = namedImport("io-ts-types/lib/readonlyNonEmptyArray", "readonlyNonEmptyArray"),
    ),
    iotsReadonlySetFromArray: ReferenceCode[Never] = ReferenceCode(
      valueType = None,
      codecType = namedImport("io-ts-types/lib/readonlySetFromArray", "ReadonlySetFromArrayC"),
      codecValue = namedImport("io-ts-types/lib/readonlySetFromArray", "readonlySetFromArray"),
    ),
    iotsNumberFromString: ReferenceCode[Never] = ReferenceCode(
      valueType = None,
      codecType = namedImport("io-ts-types/lib/NumberFromString", "NumberFromStringC"),
      codecValue = namedImport("io-ts-types/lib/NumberFromString", "NumberFromString"),
    ),
    iotsOption: ReferenceCode[Never] = ReferenceCode(
      valueType = None,
      codecType = namedImport("io-ts-types/lib/optionFromNullable", "OptionFromNullableC"),
      codecValue = namedImport("io-ts-types/lib/optionFromNullable", "optionFromNullable"),
    ),
    iotsUUID: ReferenceCode[Id] = ReferenceCode(
      valueType = namedImport("io-ts-types/lib/UUID", "UUID"),
      codecType = Generated(names("io-ts-types/lib/UUID", "UUID"), "typeof UUID"),
      codecValue = namedImport("io-ts-types/lib/UUID", "UUID"),
    ),
    iotsEither: ReferenceCode[Never] = ReferenceCode(
      valueType = None,
      codecType = namedImport("io-ts-types/lib/either", "EitherC"),
      codecValue = namedImport("io-ts-types/lib/either", "either"),
    ),
    iotsBigNumber: Option[ReferenceCode[Id]] = None,
    iotsLocalDate: Option[ReferenceCode[Id]] = None,
    iotsThese: Option[ReferenceCode[Never]] = None
  )

  /** A set of TypeScript imports available to use during code generation */
  case class Available(tsi: TsImports.Config) {
    final lazy val empty: TsImports = TsImports.empty

    val lift = Generated.lift

    export TsImports.namedImport

    private def namedImport(t: (String, String)): Generated = namedImport(t._2, t._1)

    private def optImport(o: Option[Generated], tpeName: String, cfgKey: String): Generated =
      o.getOrElse(sys.error(s"$tpeName type requested but $cfgKey import config value missing"))

    /** A helper to refer to values from a given import location */
    final class FptsUtil(imprt: tsi.type => String) {
      def apply(name: String, alias: Option[String] = None): Generated = namedImport(imprt(tsi), name, alias)
    }

    /** Helper to refer to values from the configured `fptsBoolean` location */
    final lazy val fptsBoolean = new FptsUtil(_.fptsBoolean)
    /** Helper to refer to values from the configured `fptsNumber` location */
    final lazy val fptsNumber = new FptsUtil(_.fptsNumber)
    /** Helper to refer to values from the configured `fptsString` location */
    final lazy val fptsString = new FptsUtil(_.fptsString)

    /** Helper to refer to values from the configured `fptsEither` location */
    final lazy val fptsEither = CallableImport(all(tsi.fptsEither, "E"), "E", ".", "")
    /** Helper to refer to values from the configured `fptsOption` location */
    final lazy val fptsOption = CallableImport(all(tsi.fptsOption, "O"), "O", ".", "")
    /** Helper to refer to values from the configured `fptsOrd` location */
    final lazy val fptsOrd = CallableImport(all(tsi.fptsOrd, "Ord"), "Ord", ".", "")
    /** Helper to call the configured `fptsPipe` function */
    final lazy val fptsPipe = CallableImport(namedImport(tsi.fptsPipe))
    /** Helper to refer to values from the configured `fptsReadonlyArray` location */
    final lazy val fptsReadonlyArray = CallableImport(all(tsi.fptsReadonlyArray, "RA"), "RA", ".", "")
    /** Helper to refer to values from the configured `fptsReadonlyArray` location */
    final lazy val fptsReadonlyNonEmptyArray = CallableImport(all(tsi.fptsReadonlyNonEmptyArray, "RNEA"), "RNEA", ".", "")
    /** Helper to refer to values from the configured `fptsReadonlySet` location */
    final lazy val fptsReadonlySet = CallableImport(all(tsi.fptsReadonlySet, "RS"), "RS", ".", "")
    /** Helper to refer to values from the configured `fptsThese` location */
    final lazy val fptsThese = CallableImport(all(tsi.fptsThese, "Th"), "Th", ".", "")

    /** The main `io-ts` import */
    final lazy val iotsImport = all(tsi.iots, "t")
    /** Reference to `iots.boolean` */
    final lazy val iotsBoolean = Generated(iotsImport, "t.boolean")
    /** Reference to the `iots.BooleanC` type */
    final lazy val iotsBooleanC = Generated(iotsImport, "t.BooleanC")
    /** Helper to call the `iots.brand` function */
    final lazy val iotsBrand = CallableImport(iotsImport, "t.brand")
    /** Helper to refer to the `iots.Branded` type */
    final lazy val iotsBrandedType = CallableImport.tpe(iotsImport, "t.Branded")
    /** Helper to refer to the `iots.Context` type */
    final lazy val iotsContext = Generated(iotsImport, "t.Context")
    /** Helper to refer to the `iots.Errors` type */
    final lazy val iotsErrors = Generated(iotsImport, "t.Errors")
    /** Helper to call the `iots.literal` function */
    final lazy val iotsLiteral = CallableImport(iotsImport, "t.literal")
    /** Reference to the `iots.LiteralC` type */
    final lazy val iotsLiteralC = CallableImport.tpe(iotsImport, "t.LiteralC")
    /** Reference to the `iots.Mixed` type */
    final lazy val iotsMixed = Generated(iotsImport, "t.Mixed")
    /** Reference to `iots.null` */
    final lazy val iotsNull = Generated(iotsImport, "t.null")
    /** Reference to `iots.number` */
    final lazy val iotsNumber = Generated(iotsImport, "t.number")
    /** Reference to the `iots.NumberC` type */
    final lazy val iotsNumberC = Generated(iotsImport, "t.NumberC")
    /** Helper to call the `iots.readonlyArray` function */
    final lazy val iotsReadonlyArray = CallableImport(iotsImport, "t.readonlyArray")
    /** Reference to the `iots.ReadonlyArrayC` type */
    final lazy val iotsReadonlyArrayC = CallableImport.tpe(iotsImport, "t.ReadonlyArrayC")
    /** Reference to the `iots.RecordC` type */
    final lazy val iotsRecordC = CallableImport.tpe(iotsImport, "t.RecordC")
    /** Helper to call the `iots.record` function */
    final lazy val iotsRecord = CallableImport(iotsImport, "t.record")
    /** Helper to call the `iots.strict` function */
    final lazy val iotsStrict = CallableImport(iotsImport, "t.strict")
    /** Reference to `iots.string` */
    final lazy val iotsString = Generated(iotsImport, "t.string")
    /** Reference to the `iots.StringC` type */
    final lazy val iotsStringC = Generated(iotsImport, "t.StringC")
    /** Reference to the `iots.TupleC` type */
    final lazy val iotsTupleC = CallableImport.tpe(iotsImport, "t.TupleC")
    /** Helper to call the `iots.tuple` function */
    final lazy val iotsTuple = CallableImport(iotsImport, "t.tuple")
    /** Helper to call the `iots.type` function */
    final lazy val iotsTypeFunction = CallableImport(iotsImport, "t.type")
    /** Reference to the `iots.Type` type */
    final lazy val iotsTypeType = CallableImport.tpe(iotsImport, "t.Type")
    /** Reference to the `iots.TypeC` type */
    final lazy val iotsTypeTypeC = CallableImport.tpe(iotsImport, "t.TypeC")
    /** Helper to refer to the `iots.TypeOf` type */
    final lazy val iotsTypeOf = CallableImport.tpe(iotsImport, "t.TypeOf")
    /** Reference to `iots.undefined` */
    final lazy val iotsUndefined = Generated(iotsImport, "t.undefined")
    /** Helper to call the `iots.union` function */
    final lazy val iotsUnion = CallableImport(iotsImport, "t.union")
    /** Reference to the `iots.UnionC` type */
    final lazy val iotsUnionC = CallableImport.tpe(iotsImport, "t.UnionC")
    /** Reference to `iots.unknown` */
    final lazy val iotsUnknown = Generated(iotsImport, "t.unknown")
    /** Reference to the `iots.UnknownC` type */
    final lazy val iotsUnknownC = Generated(iotsImport, "t.UnknownC")

    /** Reference to the configured `iotsDateTime` codec type */
    final lazy val iotsDateTimeType = tsi.iotsDateTime.codecType
    /** Reference to the configured `iotsDateTime` value */
    final lazy val iotsDateTimeValue = tsi.iotsDateTime.codecValue

    /** Helper to call the configured `iotsReadonlyMapFromEntries` codec type */
    final lazy val iotsReadonlyMapFromEntriesType = CallableImport.tpe(tsi.iotsReadonlyMapFromEntries.codecType)
    /** Helper to call the configured `iotsReadonlyMapFromEntries` function */
    final lazy val iotsReadonlyMapFromEntriesValue = CallableImport(tsi.iotsReadonlyMapFromEntries.codecValue)

    /** Helper to call the configured `iotsReadonlyNonEmptyArray` codec type */
    final lazy val iotsReadonlyNonEmptyArrayType = CallableImport.tpe(tsi.iotsReadonlyNonEmptyArray.codecType)
    /** Helper to call the configured `iotsReadonlyNonEmptyArray` function */
    final lazy val iotsReadonlyNonEmptyArrayValue = CallableImport(tsi.iotsReadonlyNonEmptyArray.codecValue)

    /** Helper to call the configured `iotsReadonlySetFromArray` codec type */
    final lazy val iotsReadonlySetFromArrayType = CallableImport.tpe(tsi.iotsReadonlySetFromArray.codecType)
    /** Helper to call the configured `iotsReadonlySetFromArray` function */
    final lazy val iotsReadonlySetFromArrayValue = CallableImport(tsi.iotsReadonlySetFromArray.codecValue)

    /** Reference to the configured `iotsNumberFromString` codec type */
    final lazy val iotsNumberFromStringType = tsi.iotsNumberFromString.codecType
    /** Reference to the configured `iotsNumberFromString` value */
    final lazy val iotsNumberFromStringValue = tsi.iotsNumberFromString.codecValue

    /** Helper to call the configured `iotsOption` codec type */
    final lazy val iotsOptionType = CallableImport.tpe(tsi.iotsOption.codecType)
    /** Helper to call the configured `iotsOption` function */
    final lazy val iotsOptionValue = CallableImport(tsi.iotsOption.codecValue)

    /** Reference to the configured `iotsBigNumber` codec type */
    final lazy val iotsBigNumberType = optImport(tsi.iotsBigNumber.map(_.codecType), "BigNumber", "iotsBigNumber")
    /** Reference to the configured `iotsBigNumber` value */
    final lazy val iotsBigNumberValue = optImport(tsi.iotsBigNumber.map(_.codecValue), "BigNumber", "iotsBigNumber")

    /** Helper to call the configured `iotsEither` codec type */
    final lazy val iotsEitherType = CallableImport.tpe(tsi.iotsEither.codecType)
    /** Helper to call the configured `iotsEither` function */
    final lazy val iotsEitherValue = CallableImport(tsi.iotsEither.codecValue)

    /** Reference to the configured `iotsLocalDate` codec type */
    final lazy val iotsLocalDateType = optImport(tsi.iotsLocalDate.map(_.codecType), "LocalDate", "iotsLocalDate")
    /** Reference to the configured `iotsLocalDate` value */
    final lazy val iotsLocalDateValue = optImport(tsi.iotsLocalDate.map(_.codecValue), "LocalDate", "iotsLocalDate")

    /** Helper to call the configured `iotsThese` codec type */
    final lazy val iotsTheseType = CallableImport.tpe(optImport(tsi.iotsThese.map(_.codecType), "These", "iotsThese"))
    /** Helper to call the configured `iotsThese` function */
    final lazy val iotsTheseValue = CallableImport(optImport(tsi.iotsThese.map(_.codecValue), "These", "iotsThese"))

    /** Reference to the configured `iotsUUID` codec type */
    final lazy val iotsUUIDType = tsi.iotsUUID.codecType
    /** Reference to the configured `iotsUUID` value */
    final lazy val iotsUUIDValue = tsi.iotsUUID.codecValue

    final lazy val unknownType = Generated.lift("unknown")
    final lazy val stringType = Generated.lift("string")
    final lazy val numberType = Generated.lift("number")
    final lazy val booleanType = Generated.lift("boolean")
    /** Reference to the configured `iotsDateTime` value type */
    final lazy val dateTimeType = tsi.iotsDateTime.valueType
    /** Helper to build a ReadonlyArray type */
    final lazy val readonlyArrayType = CallableImport.tpe(TsImports.empty, "ReadonlyArray")
    /** Helper to build a ReadonlyNonEmptyArray type */
    final lazy val readonlyNonEmptyArrayType = CallableImport.tpe(fptsReadonlyNonEmptyArray("ReadonlyNonEmptyArray"))
    /** Helper to build a ReadonlyMap type */
    final lazy val readonlyMapType = CallableImport.tpe(TsImports.empty, "ReadonlyMap")
    /** Helper to build a ReadonlySet type */
    final lazy val readonlySetType = CallableImport.tpe(TsImports.empty, "ReadonlySet")
    /** Helper to build an Option type */
    final lazy val optionType = CallableImport.tpe(fptsOption("Option"))
    /** Reference to the configured `iotsBigNumber` value type */
    final lazy val bigNumberType = optImport(tsi.iotsBigNumber.map(_.valueType), "BigNumber", "iotsBigNumber")
    /** Helper to build an Either type */
    final lazy val eitherType = CallableImport.tpe(fptsEither("Either"))
    /** Reference to the configured `iotsLocalDate` value type */
    final lazy val localDateType = optImport(tsi.iotsLocalDate.map(_.valueType), "LocalDate", "iotsLocalDate")
    /** Helper to build a These type */
    final lazy val theseType = CallableImport.tpe(fptsThese("These"))
    /** Reference to the configured `iotsUUID` value type */
    final lazy val uuidType = tsi.iotsUUID.valueType
    /** Helper to build a Record type */
    final lazy val recordType = CallableImport.tpe(TsImports.empty, "Record")

    private var incr = Map.empty[String, Int]

    /**
     * Import a custom type
     *
     * @param typeName The name of the type used to resolve the import
     * @param valueName The name of the value to import
     */
    final def custom(typeName: TypeName, valueName: String): Generated = {
      incr = incr.updatedWith(valueName) {
        case Some(i) => Some(i + 1)
        case None => Some(0)
      }
      namedImport(typeName, valueName, Some(s"imported${incr(valueName)}_$valueName"))
    }
  }
}
