package scalats

import cats.Eval
import scala.util.matching.Regex

/**
 * A helper to sort generated types, see [[scalats.TypeSorter.sort]]
 */
object TypeSorter {
  private case class GeneratedWithIndex(gen: Generated, index: Int)

  private val constRx = """(?:const ([^\s]+))""".r
  private val functionRx = """(?:function ([^\s<\(]+))""".r
  private val typeRx = """(?:type ([^\s<]+))""".r

  private def extractNames(gen: GeneratedWithIndex, rx: Regex): Map[String, GeneratedWithIndex] =
    rx.findAllMatchIn(gen.gen.code).toList.map(_.group(1)).map((_, gen)).toMap

  /**
   * Sort a set of generated types based on references between them
   *
   * @param types The generated types
   * @return A list of generated code sorted properly
   */
  def sort(types: List[(Option[TypeName], Generated)]): List[Generated] = {
    val gwis = types.zipWithIndex.map { case ((_, gen), idx) => GeneratedWithIndex(gen, idx) }
    val definitions = gwis.flatMap(gen =>
      extractNames(gen, constRx) ++ extractNames(gen, functionRx) ++ extractNames(gen, typeRx)
    ).toMap
    // Names can be referenced either as `foo` or `importedN_foo` where `N` is a number
    val definitionNamesRx = ("""(?:imported\d+_|\b)(""" ++
      definitions.keys.map(name => Regex.quote(name) ++ "\\b").mkString("|") ++
    ")").r

    def addGenerated(acc: Eval[(List[Generated], Set[Int])], gen: GeneratedWithIndex): Eval[(List[Generated], Set[Int])] =
      acc.flatMap { case acc @ (accGen, accSkip) =>
        if (accSkip.contains(gen.index))
          Eval.now(acc)
        else {
          val refNames = definitionNamesRx.findAllMatchIn(gen.gen.code).map(_.group(1)).toSet
          refNames.foldLeft(Eval.now((accGen, accSkip + gen.index)))(
            (acc, refName) => definitions.get(refName).fold(acc)(addGenerated(acc, _))
          ).map { case (x, y) => (x :+ gen.gen, y) }
        }
      }

    gwis.foldLeft(Eval.now(List.empty[Generated], Set.empty[Int]))(addGenerated).value._1
  }
}
