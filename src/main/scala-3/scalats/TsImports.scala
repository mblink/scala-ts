package scalats

import cats.{Monoid, Semigroup, Show}
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import java.io.File
import java.nio.file.Paths

sealed trait TsImport {
  val str: String
  override final lazy val toString: String = str
}

object TsImport {
  sealed trait Location
  case class Resolved(location: String) extends Location
  case class Unresolved(typeName: TypeName) extends Location

  implicit val semigroup: Semigroup[TsImport] = Semigroup.instance((x, y) => (x, y) match {
    case (i @ TsImport_*(_), TsImport_*(_)) => i
    case (TsImportNames(ns), TsImportNames(ns2)) => TsImportNames(ns ++ ns2)
    case (TsImport_*(_), TsImportNames(ns)) => sys.error(s"Can't import `*` and `${ns.mkString}`")
    case (TsImportNames(ns), TsImport_*(_)) => sys.error(s"Can't import `${ns.mkString}` and `*`")
  })

  implicit val show: Show[TsImport] = Show.show(_.str)
}

case class TsImport_*(alias: String) extends TsImport {
  val str: String = s"* as $alias"
}

case class TsImportNames(names: Set[String]) extends TsImport {
  val str: String = names.mkString("{ ", ", ", " }")
}
object TsImportNames {
  def apply(ns: String*): TsImportNames = new TsImportNames(ns.toSet)
}

class TsImports private (private val m: Map[TsImport.Location, TsImport]) {
  import TsImports.*

  private[TsImports] final lazy val toList: List[(TsImport.Location, TsImport)] = m.toList

  override final lazy val toString: String =
    s"TsImports(${toList.flatMap {
      case (TsImport.Resolved(l), i) => Some(s"""import $i from "$l"""")
      case (TsImport.Unresolved(_), i) => Some(s"""import $i from <unresolved>""")
    }.mkString(", ")})"

  final def ++(other: TsImports): TsImports = new TsImports(m |+| other.m)

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
        val file = typeToFile.getOrElse(t.full, sys.error(s"Failed to resolve import for type `${t.full}` from file `$currFile`"))

        if (file == currFile) {
          (accImports, i match {
            case TsImportNames(names) =>
              names.foldLeft(accCode) {
                case (acc, importedNameRx(newName, oldName)) => acc.replace(oldName, newName)
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
  lazy val empty: TsImports = new TsImports(Map())

  def all(loc: String, alias: String): TsImports =
    new TsImports(Map(TsImport.Resolved(loc) -> TsImport_*(alias)))

  def all(typeName: TypeName, alias: String): TsImports =
    new TsImports(Map(TsImport.Unresolved(typeName) -> TsImport_*(alias)))

  def names(loc: String, name1: String, otherNames: String*): TsImports =
    new TsImports(Map(TsImport.Resolved(loc) -> TsImportNames(otherNames.toSet + name1)))

  def names(typeName: TypeName, name1: String, otherNames: String*): TsImports =
    new TsImports(Map(TsImport.Unresolved(typeName) -> TsImportNames(otherNames.toSet + name1)))

  implicit val monoid: Monoid[TsImports] = Monoid.instance(empty, _ ++ _)

  def lift(s: String): Generated = Generated(empty, s)

  private val importedNameRx = "^(.*?) as (imported\\d+_\\1)$".r
  private val tsFileRx = "^(.*)\\.tsx?$".r

  private[TsImports] def relPath(filePath: String, fromPath: String): String =
    Paths.get(fromPath).relativize(Paths.get(filePath)).toString

  private[TsImports] def normalizePath(path: String): String = {
    val woExt = tsFileRx.replaceAllIn(path, "$1")
    if (woExt.startsWith(".")) woExt else s"./$woExt"
  }

  case class CallableImport(tsImports: TsImports, private val name: String, prefix: String = "(", suffix: String = ")") {
    lazy val value: Generated = Generated(tsImports, name)

    def apply(s: String*): Generated =
      Generated(tsImports, name ++ prefix ++ s.mkString(", ") ++ suffix)

    def apply(t: Generated*)(implicit d: DummyImplicit): Generated = {
      val w = lift(name ++ prefix) |+| t.intercalate(lift(", ")) |+| lift(suffix)
      Generated(tsImports ++ w.imports, w.code)
    }
  }

  object CallableImport {
    def apply(t: Generated): CallableImport = CallableImport(t._1, t._2)
    def apply(t: Generated, p: String, s: String): CallableImport = CallableImport(t._1, t._2, p, s)
  }

  case class Config(
    fptsBoolean: String = "fp-ts/lib/boolean",
    fptsDate: String = "fp-ts/lib/Date",
    fptsEither: String = "fp-ts/lib/Either",
    fptsNumber: String = "fp-ts/lib/number",
    fptsOption: String = "fp-ts/lib/Option",
    fptsOrd: String = "fp-ts/lib/Ord",
    fptsReadonlyArray: String = "fp-ts/lib/ReadonlyArray",
    fptsReadonlySet: String = "fp-ts/lib/ReadonlySet",
    fptsString: String = "fp-ts/lib/string",
    fptsThese: String = "fp-ts/lib/These",
    fptsPipe: (String, String) = ("pipe", "fp-ts/lib/function"),
    iots: String = "io-ts",
    iotsDateTime: (String, String) = ("DateFromISOString", "io-ts-types/lib/DateFromISOString"),
    iotsReadonlyNonEmptyArray: (String, String) = ("readonlyNonEmptyArray", "io-ts-types/lib/readonlyNonEmptyArray"),
    iotsReadonlySetFromArray: (String, String) = ("readonlySetFromArray", "io-ts-types/lib/readonlySetFromArray"),
    iotsNumberFromString: (String, String) = ("NumberFromString", "io-ts-types/lib/NumberFromString"),
    iotsOption: (String, String) = ("optionFromNullable", "io-ts-types/lib/optionFromNullable"),
    iotsBigNumber: Option[(String, String)] = None,
    iotsEither: Option[(String, String)] = None,
    iotsLocalDate: Option[(String, String)] = None,
    iotsThese: Option[(String, String)] = None
  )

  case class Available(tsi: TsImports.Config) {
    final lazy val empty: TsImports = TsImports.empty

    def lift(s: String): Generated = TsImports.lift(s)

    private def namedImport(loc: String, valueName: String, alias: Option[String]): Generated =
      Generated(names(loc, alias.fold(valueName)(a => s"$valueName as $a")), alias.getOrElse(valueName))

    private def namedImport(loc: String, valueName: String): Generated = namedImport(loc, valueName, None)

    private def namedImport(typeName: TypeName, valueName: String, alias: Option[String]): Generated =
      Generated(names(typeName, alias.fold(valueName)(a => s"$valueName as $a")), alias.getOrElse(valueName))

    @annotation.unused
    private def namedImport(typeName: TypeName, valueName: String): Generated = namedImport(typeName, valueName, None)

    private def namedImport(t: (String, String)): Generated = namedImport(t._2, t._1)

    private def optImport(o: Option[(String, String)], tpeName: String, cfgKey: String): Generated =
      o.map(namedImport).getOrElse(sys.error(s"$tpeName type requested but $cfgKey import config value missing"))

    class FptsUtil(imprt: tsi.type => String) {
      def apply(name: String, alias: Option[String] = None): Generated = namedImport(imprt(tsi), name, alias)
    }

    lazy val fptsBoolean = new FptsUtil(_.fptsBoolean)
    lazy val fptsDate = new FptsUtil(_.fptsDate)
    lazy val fptsNumber = new FptsUtil(_.fptsNumber)
    lazy val fptsString = new FptsUtil(_.fptsString)

    lazy val fptsEither = CallableImport(all(tsi.fptsEither, "E"), "E", ".", "")
    lazy val fptsOption = CallableImport(all(tsi.fptsOption, "O"), "O", ".", "")
    lazy val fptsOrd = CallableImport(all(tsi.fptsOrd, "Ord"), "Ord", ".", "")
    lazy val fptsPipe = CallableImport(namedImport(tsi.fptsPipe))
    lazy val fptsReadonlyArray = CallableImport(all(tsi.fptsReadonlyArray, "RA"), "RA", ".", "")
    lazy val fptsReadonlySet = CallableImport(all(tsi.fptsReadonlySet, "RS"), "RS", ".", "")
    lazy val fptsThese = CallableImport(all(tsi.fptsThese, "Th"), "Th", ".", "")

    lazy val iotsImport = all(tsi.iots, "t")
    lazy val iotsBoolean = Generated(iotsImport, "t.boolean")
    lazy val iotsBrand = CallableImport(iotsImport, "t.brand")
    lazy val iotsBrandedType = CallableImport(iotsImport, "t.Branded", "<", ">")
    lazy val iotsContext = Generated(iotsImport, "t.Context")
    lazy val iotsErrors = Generated(iotsImport, "t.Errors")
    lazy val iotsLiteral = CallableImport(iotsImport, "t.literal")
    lazy val iotsMixed = Generated(iotsImport, "t.Mixed")
    lazy val iotsNull = Generated(iotsImport, "t.null")
    lazy val iotsNumber = Generated(iotsImport, "t.number")
    lazy val iotsReadonlyArray = CallableImport(iotsImport, "t.readonlyArray")
    lazy val iotsRecord = CallableImport(iotsImport, "t.record")
    lazy val iotsStrict = CallableImport(iotsImport, "t.strict")
    lazy val iotsString = Generated(iotsImport, "t.string")
    lazy val iotsTuple = CallableImport(iotsImport, "t.tuple")
    lazy val iotsTypeFunction = CallableImport(iotsImport, "t.type")
    lazy val iotsTypeType = Generated(iotsImport, "t.Type")
    lazy val iotsTypeTypeC = Generated(iotsImport, "t.TypeC")
    lazy val iotsTypeOf = CallableImport(iotsImport, "t.TypeOf", "<", ">")
    lazy val iotsUndefined = Generated(iotsImport, "t.undefined")
    lazy val iotsUnion = CallableImport(iotsImport, "t.union")
    lazy val iotsUnknown = Generated(iotsImport, "t.unknown")

    lazy val iotsDateTime = namedImport(tsi.iotsDateTime)
    lazy val iotsReadonlyNonEmptyArray = CallableImport(namedImport(tsi.iotsReadonlyNonEmptyArray))
    lazy val iotsReadonlySetFromArray = CallableImport(namedImport(tsi.iotsReadonlySetFromArray))
    lazy val iotsNumberFromString = namedImport(tsi.iotsNumberFromString)
    lazy val iotsOption = CallableImport(namedImport(tsi.iotsOption))
    lazy val iotsBigNumber = optImport(tsi.iotsBigNumber, "BigNumber", "iotsBigNumber")
    lazy val iotsEither = CallableImport(optImport(tsi.iotsEither, "Either", "iotsEither"))
    lazy val iotsLocalDate = optImport(tsi.iotsLocalDate, "LocalDate", "iotsLocalDate")
    lazy val iotsThese = CallableImport(optImport(tsi.iotsThese, "These", "iotsThese"))

    private var incr = Map.empty[String, Int]

    def custom(typeName: TypeName, valueName: String): Generated = {
      incr = incr.updatedWith(valueName) {
        case Some(i) => Some(i + 1)
        case None => Some(0)
      }
      namedImport(typeName, valueName, Some(s"imported${incr(valueName)}_$valueName"))
    }
  }
}
