package com.mpc.scalats.core

import com.mpc.scalats.configuration.Config
import scala.reflect.runtime.universe._

/**
  * Created by Milosz on 11.06.2016.
  */
object TypeScriptGenerator {

  private def updateConfig(c: Config): Config = {
    if (c.emitIoTs) {
      c.copy(
        emitClasses = false,
        emitInterfaces = true,
        optionToNullable = false,
        optionToUndefined = false)
    } else {
      c
    }
  }

  def getClassNames(mirror: Mirror)(className: String) = {
    try {
      mirror.staticClass(className).toType
    } catch {
      case e: Throwable => {
        println(s"Could not find class $className, trying as module")
        mirror.staticModule(className).moduleClass.asType.toType
      }
    }
  }

  def generateFromClassNames(
    classNames: List[String],
    excludeClassNames: List[String],
    logger: Logger,
    classLoader: ClassLoader = getClass.getClassLoader
  )(c: Config) = {
    val mirror = runtimeMirror(classLoader)
    val types = classNames.map(cn => {
      println(s"className = $cn")
      getClassNames(mirror)(cn)
    })
    val excludeTypes = excludeClassNames.map(cn => {
      println(s"exclude className = $cn")
      getClassNames(mirror)(cn)
    })

    generate(types, excludeTypes, logger, mirror)(updateConfig(c))
  }

  def generate(caseClasses: List[Type], excludeCaseClasses: List[Type], logger: Logger, mirror: Mirror)(c: Config) = {
    implicit val config: Config = updateConfig(c)
    val outputStream = config.outputStream.getOrElse(Console.out)
    val scalaParser = new ScalaParser(logger, mirror, excludeCaseClasses)
    val scalaTypes = scalaParser.parseTypes(caseClasses)
    val typeScriptInterfaces = Compiler.compile(scalaTypes)

    outputStream.println("/**********************************************************")
    outputStream.println(" *                                                        *")
    outputStream.println(" *    FILE GENERATED BY SCALA-TS. DO NOT EDIT BY HAND.    *")
    outputStream.println(" *                                                        *")
    outputStream.println(" *********************************************************/")
    outputStream.println()

    val emitter: Emitter = if (config.emitIoTs) {
      if (config.tsImports.iots) {
        outputStream.println("""import * as t from "io-ts";""")
      }
      if (config.tsImports.iotsDate) {
        outputStream.println("""import { DateFromISOString } from "io-ts-types/lib/DateFromISOString";""")
      }
      if (config.tsImports.iotsNonEmptyArray) {
        outputStream.println("""import { nonEmptyArray } from "io-ts-types/lib/nonEmptyArray";""")
      }
      if (config.tsImports.iotsNumberFromString) {
        outputStream.println("""import { NumberFromString } from "io-ts-types/lib/NumberFromString";""")
      }
      if (config.tsImports.iotsEither) {
        outputStream.println("""import { either } from "io-ts-types/lib/either";""")
      }
      if (config.tsImports.iotsOption) {
        outputStream.println("""import { optionFromNullable } from "io-ts-types/lib/optionFromNullable";""")
      }
      config.tsImports.customImports.foreach(outputStream.println)

      outputStream.println()

      new IoTsEmitter(config)
    } else {
      new TypeScriptEmitter(config)
    }

    emitter.emit(typeScriptInterfaces, outputStream)
  }
}
