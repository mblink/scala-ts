package com.mpc.scalats.core

import com.mpc.scalats.configuration.Config
import java.io.{File, PrintStream}
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

object TypeScriptGenerator {
  def normalizeName(packagePath: Option[String], simpleName: String) =
    s"${packagePath.getOrElse("")}.${Option(simpleName).getOrElse("")}".stripPrefix(".")

  def reflectType(mirror: Mirror)(packagePath: Option[String], simpleName: String): Symbol =
    (packagePath match {
      case Some(p) => Try(mirror.staticModule(p)).getOrElse(mirror.staticPackage(p))
      case None => mirror.staticPackage("scala")
    }).typeSignature.member(TypeName(simpleName))

  def reflectModule(mirror: Mirror)(packagePath: Option[String], simpleName: String): Symbol =
    mirror.staticModule(normalizeName(packagePath, simpleName))

  def reflectClass(mirror: Mirror)(packagePath: Option[String], simpleName: String): Symbol =
    mirror.staticClass(normalizeName(packagePath, simpleName))

  val reflectTypes = List("class" -> reflectClass _, "module" -> reflectModule _, "type" -> reflectType _)

  def tryReflect(mirror: Mirror)(packagePath: Option[String], simpleName: String): Try[Symbol] = {
    val name = normalizeName(packagePath, simpleName)
    reflectTypes.zipWithIndex.foldLeft(Failure(new Throwable): Try[Symbol]) { case (acc, ((tpe, fn), i)) =>
      acc.orElse {
        reflectTypes.lift(i - 1).foreach { case (t, _) => println(s"Could not find $t $name, trying as $tpe") }
        Try(fn(mirror)(packagePath, simpleName))
      }
    }
  }

  val classNameRx = """^((?:([^\.]+(?:\.[^\.]+)*)\.)?([a-zA-Z0-9]+))(?:\[(.*)\])?$""".r

  def getTypeFromName(mirror: Mirror)(className: String): Type =
    className match {
      case classNameRx(_, packagePath, simpleName, typeParamsStr) =>
        val typeParams = ScalaParser.parseTypeParams(typeParamsStr).map(getTypeFromName(mirror))
        val tpe = tryReflect(mirror)(Option(packagePath), simpleName) match {
          case Success(m: ModuleSymbol) => m.moduleClass.asType.toType
          case Success(s) => appliedType(s, typeParams)
          case Failure(th) => throw new Throwable(s"Failed to parse type $className", th)
        }
        if (typeParams.isEmpty && tpe.typeParams.nonEmpty) tpe.typeConstructor else tpe

      case _ => sys.error(s"Something went wrong, unable to parse `$className`")
    }

  def looseTpeEq(t1: Type, t2: Type): Boolean =
    t1 =:= t2 || t1.typeSymbol == t2.typeSymbol || (t1.typeSymbol.isClass && t1.typeSymbol.asClass.isSealed && t2 <:< t1)

  class TypeToFile(pairs: List[(Type, String)]) {
    def get(tpe: Type): Option[String] =
      pairs.collect { case (t, f) if looseTpeEq(t, tpe) => f }.distinct match {
        case s :: Nil => Option(s)
        case ss @ (_ :: _ :: _) => sys.error(s"Type $tpe found in more than one file: ${ss.mkString(", ")}")
        case Nil => None
      }

    def forFile(file: String): List[Type] =
      pairs.collect { case (t, f) if f == file => t }

    lazy val allFiles: Set[String] = pairs.map(_._2).toSet
  }

  def generateFiles(
    basePath: File,
    files: Map[String, List[String]],
    logger: Logger,
    classLoader: ClassLoader = getClass.getClassLoader
  )(implicit config: Config) = {
    val mirror = runtimeMirror(classLoader)
    var classNameToType = Map[String, Type]()
    val typeToFile = new TypeToFile(files.toList.flatMap { case (f, classNames) =>
      classNames.map { n =>
        val tpe = getTypeFromName(mirror)(n)
        classNameToType = classNameToType + (n -> tpe)
        tpe -> s"$basePath/$f"
      }
    })

    files.foreach { case (fileName, _) =>
      val file = new File(s"$basePath/$fileName")
      file.getParentFile.mkdirs()
      file.createNewFile()
      val outputStream = new PrintStream(file)

      val excludeType = (t: Type) => typeToFile.get(t).exists(_ != file.toString)
      val scalaParser = new ScalaParser(logger, mirror, excludeType)
      val scalaTypes = scalaParser.parseTypes(typeToFile.forFile(file.toString))

      outputStream.println("/**********************************************************")
      outputStream.println(" *                                                        *")
      outputStream.println(" *    FILE GENERATED BY SCALA-TS. DO NOT EDIT BY HAND.    *")
      outputStream.println(" *                                                        *")
      outputStream.println(" *********************************************************/")
      outputStream.println()

      val emitter = new IoTsEmitter(config)
      val (imports, lines) = emitter.emit(Compiler(config).compile(scalaTypes))

      imports
        .resolve((tpe, name) => typeToFile.get(tpe).filter(_ != file.toString)
          .map(f => TsImports.names(f.toString, name)).getOrElse(TsImports.empty))
        .foreach(i => outputStream.println(i.asString(file, typeToFile.allFiles)))
      outputStream.println()

      lines.foreach(outputStream.println)
      outputStream.println()
    }
  }
}
