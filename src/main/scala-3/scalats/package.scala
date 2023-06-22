package scalats

import cats.syntax.foldable.*
import java.io.{File, PrintStream}
import scala.quoted.*
import scala.util.Using

private def parseImpl[A: Type](using Quotes): Expr[TsModel] =
  new TsParser().parseTopLevel[A]

inline def parse[A]: TsModel = ${ parseImpl[A] }

private def parseReferenceImpl[A: Type](using Quotes): Expr[TsModel] =
  new TsParser().parse[A](false)

inline def parseReference[A]: TsModel = ${ parseReferenceImpl[A] }

def generateAll(all: Map[File, List[TsModel]])(
  using customType: TsCustomType,
  imports: TsImports.Available,
): List[(File, (List[(Option[TypeName], Generated)], ReferencedTypes))] = {
  val generator = new TsGenerator(customType, imports)
  all.toList.foldMap { case (file, models) =>
    Map(file.toString -> (models.flatMap(generator.generateTopLevel), models.foldMap(generator.referencedTypes)))
  }.toList.map { case (file, (types, refs)) => (new File(file), (types.distinctBy(_._1.map(_.full)), refs)) }
}

private def withFileWriter[A](file: File)(f: PrintStream => A): A = {
  new File(file.getParent).mkdirs
  Using.resource(new PrintStream(file))(f)
}

private def mkAllTypesByFile(all: List[(File, (List[(Option[TypeName], Generated)], ReferencedTypes))]): Map[String, Set[TypeName]] =
  all.foldMap { case (f, ts) => Map(f.toString -> ts._1.flatMap(_._1).toSet) }

def writeAll(all: List[(File, (List[(Option[TypeName], Generated)], ReferencedTypes))]): Unit = {
  val allTypesByFile = mkAllTypesByFile(all)
  val res = all.map { case (file, (types, refs)) =>
    val (allImports, allCode) = TypeSorter.sort(types, refs).foldMap { case Generated(imports, code) =>
      val (updImports, updCode) = imports.resolve(file.toString, allTypesByFile, code)
      (updImports, updCode + "\n\n")
    }
    (file, allImports.map { case (loc, imprt) =>
      s"""import ${imprt.str} from "${loc.location}";"""
    }.mkString("\n") + "\n\n" + allCode)
  }
  res.foreach { case (file, code) => withFileWriter(file)(_.print(code)) }
}

def writeAll(all: Map[File, List[TsModel]])(using customType: TsCustomType, imports: TsImports.Available): Unit =
  writeAll(generateAll(all))

def resolve(
  currFile: File,
  generated: Generated,
  allGenerated: List[(File, (List[(Option[TypeName], Generated)], ReferencedTypes))],
): (Map[TsImport.Resolved, TsImport], String) = {
  val allTypesByFile = mkAllTypesByFile(allGenerated)
  val Generated(imports, code) = generated
  imports.resolve(currFile.toString, allTypesByFile, code)
}

def referenceCode(model: TsModel)(
  using customType: TsCustomType,
  imports: TsImports.Available,
): Generated = {
  val generator = new TsGenerator(customType, imports)
  generator.generate(generator.GenState(false, identity), model).foldMap(_._2)
}
