package com.mpc.scalats.core

import cats.{Monoid, Semigroup}
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.semigroup._
import com.mpc.scalats.configuration.Config
import java.io.File
import java.nio.file.Paths
import scala.reflect.runtime.universe.Type

sealed trait TsImport {
  val str: String
  def fold[A](f: TsImport_* => A, g: TsImportNames => A): A = this match {
    case x @ TsImport_*(_) => f(x)
    case x @ TsImportNames(_) => g(x)
  }
}

object TsImport {
  implicit val semigroup: Semigroup[TsImport] = Semigroup.instance((x, y) => (x, y) match {
    case (TsImport_*(_), i @ TsImport_*(_)) => i
    case (TsImportNames(ns), TsImportNames(ns2)) => TsImportNames(ns ++ ns2)
    case (TsImport_*(_), TsImportNames(ns)) => sys.error(s"Can't import `*` and `${ns.mkString}`")
    case (TsImportNames(ns), TsImport_*(_)) => sys.error(s"Can't import `${ns.mkString}` and `*`")
  })
}

case class TsImport_*(alias: String) extends TsImport {
  val str: String = s"* as $alias"
}

case class TsImportNames(names: Set[String]) extends TsImport {
  val str: String = names.mkString("{ ", ", ", "}")
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

  def asString(currFile: File, allFiles: Set[String]): String =
    "import " ++ run.fold(x => s"* as ${x.alias}", x => x.names.mkString("{ ", ", ", " }")) ++ """ from """" ++
      // Relativize paths to files that exist on the file system or are currently being generated
      (if (file.exists || allFiles.contains(file.toString))
        normalizePath(relPath(loc, currFile.getParent).replaceAll("\\.tsx?$", ""))
      else loc) ++ """";"""
}

class TsImports private (
  private val m: Map[String, TsImport],
  val needed: List[(Type, String)]
) {
  def ++(other: TsImports): TsImports =
    new TsImports(m |+| other.m, needed ++ other.needed)

  def resolve(customType: (Type, String) => TsImports): TsImports.Resolved =
    new TsImports.Resolved((this ++ needed.foldMap(customType.tupled(_).resolve(customType): TsImports)).m)
}


object TsImports {
  lazy val empty: TsImports = new TsImports(Map(), Nil)

  def all(loc: String, alias: String): TsImports =
    new TsImports(Map(loc -> TsImport_*(alias)), Nil)

  def names(loc: String, name1: String, otherNames: String*): TsImports =
    new TsImports(Map(loc -> TsImportNames(otherNames.toSet + name1)), Nil)

  def needed(names: List[(Type, String)]): TsImports =
    new TsImports(Map(), names)

  implicit val monoid: Monoid[TsImports] = Monoid.instance(empty, _ ++ _)

  def join[A](values: Iterable[A], prefix: String, glue: String, suffix: String)(
    f: A => TsImports.With[String]
  ): TsImports.With[String] =
    values.toList.foldMap(f(_).map(List(_))).map(_.mkString(prefix, glue, suffix))

  def joinLines[A](values: Iterable[A], glue: String)(f: A => TsImports.With[String]): TsImports.With[List[String]] = {
    val l = values.toList.zipWithIndex
    val last = l.length - 1
    l.foldMap { case (a, i) => f(a).map(s => List(s ++ (if (i == last) "" else glue))) }
  }

  class Resolved(m: Map[String, TsImport]) extends TsImports(m, Nil) {
    def foreach(f: QualifiedImport => Unit): Unit =
      m.foreach { case (l, i) => f(QualifiedImport(l, i)) }
  }

  type With[A] = (TsImports, A)

  trait WithOps {
    implicit class WithTsImportsOps[A](t: TsImports.With[A]) {
      def :+(a: A)(implicit S: Semigroup[A]): TsImports.With[A] = (t._1, S.combine(t._2, a))
      def +:(a: A)(implicit S: Semigroup[A]): TsImports.With[A] = (t._1, S.combine(a, t._2))
    }

    implicit class AOps[A](a: A) {
      def |+(t: TsImports.With[A])(implicit S: Semigroup[A]): TsImports.With[A] = (t._1, S.combine(a, t._2))
    }
  }

  case class CallableImport(tsImports: TsImports, private val name: String, prefix: String = "(", suffix: String = ")") {
    lazy val value: With[String] = (tsImports, name)

    def apply(s: String): With[String] = (tsImports, name ++ prefix ++ s ++ suffix)
    def apply(t: With[String]): With[String] = (tsImports ++ t._1, name ++ prefix ++ t._2 ++ suffix)

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
    lazy val empty: TsImports = TsImports.empty
    lazy val tsi = config.tsImports

    private def namedImport(t: (String, String)): With[String] = (names(t._2, t._1), t._1)
    private def optImport(o: Option[(String, String)], tpeName: String, cfgKey: String): With[String] =
      o.map(namedImport).getOrElse(sys.error(s"$tpeName type requested but $cfgKey import config value missing"))

    lazy val fptsEither = CallableImport(all(tsi.fptsEither, "E"), "E", ".", "")
    lazy val fptsPipe = CallableImport(namedImport(tsi.fptsPipe))
    lazy val fptsThese = CallableImport(all(tsi.fptsThese, "Th"), "Th", ".", "")

    lazy val iotsImport = all(tsi.iots, "t")
    lazy val iotsArray = CallableImport(iotsImport, "t.array")
    lazy val iotsBoolean = (iotsImport, "t.boolean")
    lazy val iotsErrors = (iotsImport, "t.Errors")
    lazy val iotsLiteral = CallableImport(iotsImport, "t.literal")
    lazy val iotsMixed = (iotsImport, "t.Mixed")
    lazy val iotsNull = (iotsImport, "t.null")
    lazy val iotsNumber = (iotsImport, "t.number")
    lazy val iotsRecord = CallableImport(iotsImport, "t.record")
    lazy val iotsStrict = CallableImport(iotsImport, "t.strict")
    lazy val iotsString = (iotsImport, "t.string")
    lazy val iotsTuple = CallableImport(iotsImport, "t.tuple")
    lazy val iotsTypeFunction = CallableImport(iotsImport, "t.type")
    lazy val iotsTypeType = (iotsImport, "t.Type")
    lazy val iotsTypeOf = (iotsImport, "t.TypeOf")
    lazy val iotsUndefined = (iotsImport, "t.undefined")
    lazy val iotsUnion = CallableImport(iotsImport, "t.union")


    lazy val iotsDateTime = namedImport(tsi.iotsDateTime)
    lazy val iotsNonEmptyArray = CallableImport(namedImport(tsi.iotsNonEmptyArray))
    lazy val iotsNumberFromString = namedImport(tsi.iotsNumberFromString)
    lazy val iotsEither = CallableImport(namedImport(tsi.iotsEither))
    lazy val iotsOption = CallableImport(namedImport(tsi.iotsOption))
    lazy val iotsLocalDate = optImport(tsi.iotsLocalDate, "LocalDate", "iotsLocalDate")
    lazy val iotsThese = CallableImport(optImport(tsi.iotsThese, "These", "iotsThese"))

    def custom(scalaType: Type, name: String): With[String] = (needed(List(scalaType -> name)), name)
  }
}
