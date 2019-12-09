package com.mpc.scalats.core

import com.mpc.scalats.configuration.Config
import com.mpc.scalats.core.TypeScriptModel.{Declaration, TypeRef}
import java.io.PrintStream
import scala.collection.immutable.ListSet

trait Emitter {
  val config: Config
  def emit(declaration: ListSet[Declaration], out: PrintStream): Unit
  def getTypeRefString(typeRef: TypeRef): String

  val indent: String = config.typescriptIndent
  def indent(num: Int): String = config.typescriptIndent * num
  def typeParameters(params: ListSet[String]): String =
    if (params.isEmpty) "" else params.mkString("<", ", ", ">")
}
