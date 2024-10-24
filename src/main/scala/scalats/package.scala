package scalats

import cats.syntax.foldable.*
import java.io.{File, PrintStream}
import scala.quoted.*
import scala.util.Using

private def parseImpl[A: Type](using Quotes): Expr[TsModel] =
  new TsParser().parseTopLevel[A]

/** Parse `A` into a [[scalats.TsModel]]` under the assumption that we want to define `A` */
inline def parse[A]: TsModel = ${ parseImpl[A] }

private def parseReferenceImpl[A: Type](using Quotes): Expr[TsModel] =
  new TsParser().parse[A](false)

/** Parse `A` into a [[scalats.TsModel]]` under the assumption we only want to refer to `A`, not define it */
inline def parseReference[A]: TsModel = ${ parseReferenceImpl[A] }

/**
 * Generate code for the given types
 *
 * The returned value is an intermediate representation with little value on its own.
 * It can be passed to [[scalats.writeAll]] to actually write the generated files
 * or to [[scalats.resolve]] to resolve import references for some generated code.
 *
 * If you don't need the intermediate representation to pass to [[scalats.resolve]] you can
 * use the overload of [[scalats.writeAll]] that takes the `Map[File, List[TsModel]]` directly.
 *
 * @param all A mapping of files to parsed types
 * @return The generated types
 */
def generateAll(all: Map[File, List[TsModel]], debug: Boolean = false, debugFilter: String = "")(
  using customType: TsCustomType,
  customOrd: TsCustomOrd,
  imports: TsImports.Available,
): List[(File, List[(Option[TypeName], Generated)])] = {
  val generator = new TsGenerator(customType, customOrd, imports, debug, debugFilter)
  all.toList.foldMap { case (file, models) =>
    Map(file.toString -> models.flatMap(generator.generateTopLevel))
  }.toList.map { case (file, types) => (new File(file), types.distinctBy(_._1.map(_.full))) }
}

private def withFileWriter[A](file: File)(f: PrintStream => A): A = {
  new File(file.getParent).mkdirs
  Using.resource(new PrintStream(file))(f)
}

private def mkAllTypesByFile(all: List[(File, List[(Option[TypeName], Generated)])]): Map[String, Set[TypeName]] =
  all.foldMap { case (f, ts) => Map(f.toString -> ts.flatMap(_._1).toSet) }

/**
 * Write the types generated by [[scalats.generateAll]] to the file system
 *
 * @param all The generated types
 */
def writeAll(all: List[(File, List[(Option[TypeName], Generated)])]): Unit = {
  val allTypesByFile = mkAllTypesByFile(all)
  val res = all.map { case (file, types) =>
    val (allImports, allCode) = TypeSorter.sort(types).foldMap { case Generated(imports, code) =>
      val (updImports, updCode) = imports.resolve(file.toString, allTypesByFile, code)
      (updImports, updCode + "\n\n")
    }
    (file, allImports.map { case (loc, imprt) =>
      s"""import ${imprt.str} from "${loc.location}";"""
    }.mkString("\n") + "\n\n" + allCode)
  }
  res.foreach { case (file, code) => withFileWriter(file)(_.print(code)) }
}

/**
 * Write the given types to the file system
 *
 * @param all A mapping of files to parsed types
 */
def writeAll(all: Map[File, List[TsModel]], debug: Boolean = false, debugFilter: String = "")(
  using customType: TsCustomType,
  customOrd: TsCustomOrd,
  imports: TsImports.Available,
): Unit =
  writeAll(generateAll(all, debug, debugFilter))

/**
 * Resolve import references for the given generated code
 *
 * @param currFile The current file being generated
 * @param generated The generated code with imports to be resolved
 * @param allGenerated All generated types
 * @return A set of resolve imports and code
 */
def resolve(
  currFile: File,
  generated: Generated,
  allGenerated: List[(File, List[(Option[TypeName], Generated)])],
): (Map[TsImport.Resolved, TsImport], String) = {
  val allTypesByFile = mkAllTypesByFile(allGenerated)
  val Generated(imports, code) = generated
  imports.resolve(currFile.toString, allTypesByFile, code)
}

/**
 * Generate the code necessary to refer to a given type in TypeScript code
 *
 * @param model The type to refer to
 * @return The generated code
 */
def referenceCode(model: TsModel)(
  using customType: TsCustomType,
  customOrd: TsCustomOrd,
  imports: TsImports.Available,
): Generated = {
  val generator = new TsGenerator(customType, customOrd, imports)
  generator.generate(generator.State(false, generator.WrapCodec.id), model).foldMap(_._2)
}