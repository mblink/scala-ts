package scalats

import cats.{Monoid, Semigroup, Show}
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.semigroup._

case class TypeName(name: String) {
  override final lazy val toString: String = name
}

sealed trait TsImport {
  val str: String
  override final lazy val toString: String = str
}

object TsImport {
  sealed trait Location
  case class Resolved(location: String) extends Location
  case class Unresolved(typeName: TypeName) extends Location

  implicit val semigroup: Semigroup[TsImport] = Semigroup.instance((x, y) => (x, y) match {
    case (TsImport_*(_), i @ TsImport_*(_)) => i
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

// case class QualifiedImport(loc: String, run: TsImport) {
//   lazy val file = new File(loc)

//   private def relPath(filePath: String, fromPath: String): String =
//     Paths.get(fromPath).relativize(Paths.get(filePath)).toString

//   private def normalizePath(path: String): String = {
//     val woExt = path.replaceAll("\\.tsx?$", "")
//     if (woExt.startsWith(".")) woExt else s"./$woExt"
//   }

//   def path(currFile: File, allFiles: Set[String]): String =
//     // Relativize paths to files that exist on the file system or are currently being generated
//     if (file.exists || allFiles.contains(file.toString)) normalizePath(relPath(loc, currFile.getParent))
//     else loc

//   def asString(currFile: File, allFiles: Set[String]): Option[String] =
//     if (file == currFile) None
//     else Some(show"""import $run from """" ++ path(currFile, allFiles) ++ """";""")
// }

class TsImports private (private val m: Map[TsImport.Location, TsImport]) {
  private[TsImports] final lazy val toList: List[(TsImport.Location, TsImport)] = m.toList

  override final lazy val toString: String =
    s"TsImports(${toList.map {
      case (TsImport.Resolved(l), i) => s"""import $i from "$l""""
      case (TsImport.Unresolved(_), i) => s"""import $i from <unresolved>"""
    }.mkString(", ")})"

  def ++(other: TsImports): TsImports = new TsImports(m |+| other.m)

  // def foldLeft[A](init: A)(f: (A, QualifiedImport) => A): A =
  //   m.foldLeft(init) { case (acc, (l, i)) => f(acc, QualifiedImport(l, i)) }

  // def foreach(f: QualifiedImport => Unit): Unit =
  //   foldLeft(())((_, i) => f(i))
}


object TsImports {
  lazy val empty: TsImports = new TsImports(Map())

  def all(loc: String, alias: String): TsImports =
    new TsImports(Map(TsImport.Resolved(loc) -> TsImport_*(alias)))

  def names(loc: String, name1: String, otherNames: String*): TsImports =
    new TsImports(Map(TsImport.Resolved(loc) -> TsImportNames(otherNames.toSet + name1)))

  def names(typeName: TypeName, name1: String, otherNames: String*): TsImports =
    new TsImports(Map(TsImport.Unresolved(typeName) -> TsImportNames(otherNames.toSet + name1)))

  implicit val monoid: Monoid[TsImports] = Monoid.instance(empty, _ ++ _)

  type With[A] = (TsImports, A)

  def lift[A](a: A): TsImports.With[A] = (empty, a)

  case class CallableImport(tsImports: TsImports, private val name: String, prefix: String = "(", suffix: String = ")") {
    lazy val value: With[String] = (tsImports, name)

    def apply(s: String*): With[String] =
      (tsImports, name ++ prefix ++ s.mkString(", ") ++ suffix)

    def apply(t: With[String]*)(implicit d: DummyImplicit): With[String] = {
      val (i, s) = lift(name ++ prefix) |+| t.intercalate(lift(", ")) |+| lift(suffix)
      (tsImports ++ i, s)
    }

    def lines(
      firstLine: String => With[String],
      t: With[List[String]],
      lastLine: String => With[String]
    ): With[List[String]] = {
      val (fli, fl) = firstLine(name ++ prefix)
      val (lli, ll) = lastLine(suffix)
      (tsImports ++ t._1 ++ fli ++ lli, List(fl) ++ t._2 ++ List(ll))
    }
  }

  object CallableImport {
    def apply(t: With[String]): CallableImport = CallableImport(t._1, t._2)
    def apply(t: With[String], p: String, s: String): CallableImport = CallableImport(t._1, t._2, p, s)
  }

  case class available(config: Config) {
    final lazy val empty: TsImports = TsImports.empty
    final lazy val tsi = config.tsImports

    def lift(s: String): With[String] = TsImports.lift(s)
    lazy val emptyStr: With[String] = lift("")

    private def namedImport(loc: String, valueName: String, alias: Option[String]): With[String] =
      (names(loc, alias.fold(valueName)(a => s"$valueName as $a")), alias.getOrElse(valueName))

    private def namedImport(loc: String, valueName: String): With[String] = namedImport(loc, valueName, None)

    private def namedImport(typeName: TypeName, valueName: String, alias: Option[String]): With[String] =
      (names(typeName, alias.fold(valueName)(a => s"$valueName as $a")), alias.getOrElse(valueName))

    @annotation.unused
    private def namedImport(typeName: TypeName, valueName: String): With[String] = namedImport(typeName, valueName, None)

    private def namedImport(t: (String, String)): With[String] = namedImport(t._2, t._1)

    // private def namedImport(t: (String, String)): With[String] = namedImport(t._2, t._1)

    private def optImport(o: Option[(String, String)], tpeName: String, cfgKey: String): With[String] =
      o.map(namedImport).getOrElse(sys.error(s"$tpeName type requested but $cfgKey import config value missing"))

    class FptsUtil(imprt: tsi.type => String) {
      def apply(name: String, alias: Option[String] = None): With[String] = namedImport(imprt(tsi), name, alias)
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
    lazy val iotsBoolean = (iotsImport, "t.boolean")
    lazy val iotsBrand = CallableImport(iotsImport, "t.brand")
    lazy val iotsBrandedType = CallableImport(iotsImport, "t.Branded", "<", ">")
    lazy val iotsErrors = (iotsImport, "t.Errors")
    lazy val iotsLiteral = CallableImport(iotsImport, "t.literal")
    lazy val iotsMixed = (iotsImport, "t.Mixed")
    lazy val iotsNull = (iotsImport, "t.null")
    lazy val iotsNumber = (iotsImport, "t.number")
    lazy val iotsReadonlyArray = CallableImport(iotsImport, "t.readonlyArray")
    lazy val iotsRecord = CallableImport(iotsImport, "t.record")
    lazy val iotsStrict = CallableImport(iotsImport, "t.strict")
    lazy val iotsString = (iotsImport, "t.string")
    lazy val iotsTuple = CallableImport(iotsImport, "t.tuple")
    lazy val iotsTypeFunction = CallableImport(iotsImport, "t.type")
    lazy val iotsTypeType = (iotsImport, "t.Type")
    lazy val iotsTypeTypeC = (iotsImport, "t.TypeC")
    lazy val iotsTypeOf = CallableImport(iotsImport, "t.TypeOf", "<", ">")
    lazy val iotsUndefined = (iotsImport, "t.undefined")
    lazy val iotsUnion = CallableImport(iotsImport, "t.union")
    lazy val iotsUnknown = (iotsImport, "t.unknown")

    lazy val iotsDateTime = namedImport(tsi.iotsDateTime)
    lazy val iotsReadonlyNonEmptyArray = CallableImport(namedImport(tsi.iotsReadonlyNonEmptyArray))
    lazy val iotsReadonlySetFromArray = CallableImport(namedImport(tsi.iotsReadonlySetFromArray))
    lazy val iotsNumberFromString = namedImport(tsi.iotsNumberFromString)
    lazy val iotsOption = CallableImport(namedImport(tsi.iotsOption))
    lazy val iotsBigNumber = optImport(tsi.iotsBigNumber, "BigNumber", "iotsBigNumber")
    lazy val iotsEither = CallableImport(optImport(tsi.iotsEither, "Either", "iotsEither"))
    lazy val iotsLocalDate = optImport(tsi.iotsLocalDate, "LocalDate", "iotsLocalDate")
    lazy val iotsThese = CallableImport(optImport(tsi.iotsThese, "These", "iotsThese"))

    def custom(typeName: TypeName, valueName: String): With[String] = namedImport(typeName, valueName)
  }
}
