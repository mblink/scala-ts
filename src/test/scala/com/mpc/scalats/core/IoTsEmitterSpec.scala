package com.mpc.scalats.core

import scala.collection.immutable.ListSet

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.mpc.scalats.configuration.Config

final class IoTsEmitterSpec extends AnyFlatSpec with Matchers {
  import TypeScriptModel._
  import CompilerResults._
  import ScalaFixtures._

  it should "emit IO-TS validation and type for a class with one primitive member" in {
    emit(ListSet(interface1)) should equal(
    """import * as t from "io-ts";
      |
      |export const iTestClass1 = t.type({
      |	name: t.string,
      |});
      |export type ITestClass1 = t.TypeOf<typeof iTestClass1>;
      |
      |""".stripMargin)
  }

  it should "emit IO-TS validation for a class with generic member" in {
    emit(ListSet(interface4)) should equal(
    """import * as t from "io-ts";
      |
      |export const iTestClass4 = <T extends t.Mixed>(_Tval: T) => t.type({
      |	name: iTestClass3(_Tval),
      |});
      |
      |""".stripMargin)
  }

  private lazy val ioTsConfig = Config(emitIoTs = true)

  def emit(
            decls: ListSet[Declaration],
            config: Config = ioTsConfig): String = {
    val emitter = new IoTsEmitter(config)
    val buf = new java.io.ByteArrayOutputStream()
    lazy val out = new java.io.PrintStream(buf)

    try {
      emitter.emit(decls, out)
      out.flush()
      buf.toString
    } finally {
      out.close()
    }
  }
}
