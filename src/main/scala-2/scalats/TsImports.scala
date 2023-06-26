package scalats

import cats.{Monoid, Semigroup, Show}
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.semigroup._
import cats.syntax.show._
import java.io.File
import java.nio.file.Paths
import scala.reflect.runtime.universe.Type

sealed trait TsImport {
  val str: String
}

object TsImport {
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

case class QualifiedImport(loc: String, run: TsImport) {
  lazy val file = new File(loc)

  private def relPath(filePath: String, fromPath: String): String =
    Paths.get(fromPath).relativize(Paths.get(filePath)).toString

  private def normalizePath(path: String): String = {
    val woExt = path.replaceAll("\\.tsx?$", "")
    if (woExt.startsWith(".")) woExt else s"./$woExt"
  }

  def path(currFile: File, allFiles: Set[String]): String =
    // Relativize paths to files that exist on the file system or are currently being generated
    if (file.exists || allFiles.contains(file.toString)) normalizePath(relPath(loc, currFile.getParent))
    else loc

  def asString(currFile: File, allFiles: Set[String]): Option[String] =
    if (file == currFile) None
    else Some(show"""import $run from """" ++ path(currFile, allFiles) ++ """";""")
}

class TsImports private (private val m: Map[String, TsImport]) {
  private[TsImports] def toList: List[(String, TsImport)] = m.toList

  def ++(other: TsImports): TsImports = new TsImports(m |+| other.m)

  def foldLeft[A](init: A)(f: (A, QualifiedImport) => A): A =
    m.foldLeft(init) { case (acc, (l, i)) => f(acc, QualifiedImport(l, i)) }

  def foreach(f: QualifiedImport => Unit): Unit =
    foldLeft(())((_, i) => f(i))
}

object TsImports {
  lazy val empty: TsImports = new TsImports(Map())

  def all(loc: String, alias: String): TsImports =
    new TsImports(Map(loc -> TsImport_*(alias)))

  def names(loc: String, name1: String, otherNames: String*): TsImports =
    new TsImports(Map(loc -> TsImportNames(otherNames.toSet + name1)))

  implicit val monoid: Monoid[TsImports] = Monoid.instance(empty, _ ++ _)

  type With[A] = (TsImports, A)

  trait HelperSyntax {
    implicit class TsImportsWithStringOps(s: String) {
      def |+|(t: TsImports.With[String]): TsImports.With[String] =
        (t._1, s ++ t._2)
    }

    implicit class TsImportsWithListStringOps(ls: List[String]) {
      def |+|(t: TsImports.With[List[String]]): TsImports.With[List[String]] =
        (t._1, ls ++ t._2)
    }

    implicit class TsImportsWithAOps[A](t: TsImports.With[A]) {
      def |+|(t2: TsImports.With[A])(implicit S: Semigroup[A]): TsImports.With[A] =
        (t._1 |+| t2._1, S.combine(t._2, t2._2))
    }

    implicit class TsImportsWithListAOps[A](t: TsImports.With[List[A]]) {
      def +:(a: A)(implicit S: Semigroup[A]): TsImports.With[List[A]] =
        t match {
          case (i, h :: t) => (i, S.combine(a, h) :: t)
          case x @ (_, Nil) => x
        }

      def :+(a: A)(implicit S: Semigroup[A]): TsImports.With[List[A]] =
        t match {
          case x @ (_, Nil) => x
          case (i, l) => (i, l.init :+ S.combine(l.last, a))
        }

      def +:(ta: TsImports.With[A])(implicit S: Semigroup[A]): TsImports.With[List[A]] =
        (ta, t) match {
          case ((i1, a), (i2, h :: t)) => (i1 |+| i2, S.combine(a, h) :: t)
          case (_, x @ (_, Nil)) => x
        }

      def :+(ta: TsImports.With[A])(implicit S: Semigroup[A]): TsImports.With[List[A]] =
        (t, ta) match {
          case (x @ (_, Nil), _) => x
          case ((i1, l), (i2, a)) => (i1 |+| i2, l.init :+ S.combine(l.last, a))
        }
    }

    implicit class TsImportsIterableOps[A](values: Iterable[A]) {
      def join(prefix: String, glue: String, suffix: String)(f: A => TsImports.With[String]): TsImports.With[String] =
        values.toList.foldMap(f(_).map(List(_))).map(_.mkString(prefix, glue, suffix))

      def join(glue: String)(f: A => TsImports.With[String]): TsImports.With[String] =
        join("", glue, "")(f)

      def joinArray(f: A => TsImports.With[String]): TsImports.With[String] =
        join("[", ", ", "]")(f)

      def joinParens(glue: String)(f: A => TsImports.With[String]): TsImports.With[String] =
        join("(", glue, ")")(f)

      def joinTypeParams(f: A => TsImports.With[String]): TsImports.With[String] =
        if (values.isEmpty) (empty, "") else join("<", ", ", ">")(f)

      def joinLines(glue: String)(f: A => TsImports.With[String]): TsImports.With[List[String]] = {
        val l = values.toList.zipWithIndex
        val last = l.length - 1
        l.foldMap { case (a, i) => f(a).map(s => List(s ++ (if (i == last) "" else glue))) }
      }
    }
  }

  case class CallableImport(tsImports: TsImports, private val name: String, prefix: String = "(", suffix: String = ")")
  extends HelperSyntax {
    lazy val value: With[String] = (tsImports, name)

    def apply(s: String*): With[String] =
      (tsImports, name ++ prefix ++ s.mkString(", ") ++ suffix)

    def apply(t: With[String]*)(implicit d: DummyImplicit): With[String] = {
      val (i, s) = name |+| t.join(prefix, ", ", suffix)(identity)
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

  case class Ctx(
    custom: (Type, String) => Option[TsImports.With[String]]
  )

  case class available(config: Config) {
    lazy val empty: TsImports = TsImports.empty
    lazy val tsi = config.tsImports

    def lift(s: String): With[String] = (empty, s)
    lazy val emptyStr: With[String] = lift("")

    private def namedImport(loc: String, name: String, alias: Option[String] = None): With[String] =
      (names(loc, alias.fold(name)(a => s"$name as $a")), alias.getOrElse(name))
    private def namedImport(t: (String, String)): With[String] = namedImport(t._2, t._1)
    private def optImport(o: Option[(String, String)], tpeName: String, cfgKey: String): With[String] =
      o.map(namedImport).getOrElse(sys.error(s"$tpeName type requested but $cfgKey import config value missing"))

    def fptsOrdInstance(typeRef: TypeScriptModel.TypeRef)(implicit ctx: Ctx): With[String] = {
      @annotation.nowarn("msg=dead code") lazy val err = sys.error(s"`Ord` instance requested for $typeRef but not found")

      // First check `getOrdInstance` config to see if user has specified an `Ord` instance for the given type
      config.getOrdInstance.applyOrElse(typeRef, (_: TypeScriptModel.TypeRef) match {
        // `Ord` instances that are built into fp-ts
        case TypeScriptModel.ArrayRef(t) => fptsReadonlyArray(lift("Ord(") |+| fptsOrdInstance(t) |+| lift(")"))
        case TypeScriptModel.BooleanRef => fptsBoolean("Ord", Some("boolOrd"))
        case TypeScriptModel.DateRef | TypeScriptModel.DateTimeRef => fptsDate("Ord", Some("dateOrd"))
        case TypeScriptModel.NumberRef => fptsNumber("Ord", Some("numberOrd"))
        case TypeScriptModel.OptionType(t) => fptsOption(lift("Ord(") |+| fptsOrdInstance(t) |+| lift(")"))
        case TypeScriptModel.StringRef => fptsString("Ord", Some("stringOrd"))

        // Union types being generated have an `Ord` instance defined, import it from the same file as the type itself
        case TypeScriptModel.UnionType(name, _, _, tpe) =>
          ctx.custom(tpe, "ord").map(_._1.toList).collect {
            case (file, TsImportNames(_)) :: Nil => namedImport(file, unionOrdName(name))
          }.getOrElse(err)

        // No sane `Ord` instances for these, would have have to be handled by the `getOrdInstance` config value
        case (
          TypeScriptModel.JsonRef |
          TypeScriptModel.BigNumberRef |
          TypeScriptModel.CustomTypeRef(_, _, _) |
          TypeScriptModel.EitherType(_, _) |
          TypeScriptModel.MapType(_, _) |
          TypeScriptModel.NonEmptyArrayRef(_) |
          TypeScriptModel.NullRef |
          TypeScriptModel.SetRef(_) |
          TypeScriptModel.SimpleTypeRef(_) |
          TypeScriptModel.TheseType(_, _) |
          TypeScriptModel.TupleType(_) |
          TypeScriptModel.UndefinedRef |
          TypeScriptModel.UnknownTypeRef(_, _)
        ) => err
      })
    }

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

    def custom(scalaType: Type, name: String)(implicit ctx: Ctx): Option[With[String]] = ctx.custom(scalaType, name)
  }
}
