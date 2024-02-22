package scalats
package tests

import io.circe.{Decoder, Encoder, parser}
import io.circe.syntax.*
import java.io.File
import java.util.concurrent.atomic.AtomicReference
import munit.ScalaCheckSuite
import scala.sys.process.*
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}

abstract class CodecTest[DecodeEncodeType: Arbitrary: Decoder: Encoder](
  outputDir: File,
  typesAndExpected: Map[String, (List[TsModel], String)],
  decodeEncodeCodecName: String,
  decodeEncodeCodecValue: String,
  decodeEncodeCodecFile: String,
) extends ScalaCheckSuite {
  private val types = typesAndExpected.map { case (f, (ts, _)) => (outputDir / f) -> ts }
  private val expected = typesAndExpected.map { case (f, (_, s)) => (outputDir / f).toString -> s }
  private val decodeEncodeScript = outputDir / "decode-encode.ts"

  private val code = new Fixture[Map[String, String]]("code") {
    private val ref = new AtomicReference[Map[String, String]]

    def apply() = ref.get

    override def beforeAll() = {
      writeAll(types)
      writeDecodeEncodeScript(decodeEncodeScript, decodeEncodeCodecName, decodeEncodeCodecValue, outputDir / decodeEncodeCodecFile)
      ref.set(types.map { case (f, _) => f.toString -> f.read.trim })
    }

    override def afterAll() = {
      types.foreach { case (f, _) => f.delete }
      decodeEncodeScript.delete
      ()
    }
  }

  override def munitFixtures = List(code)
  override def scalaCheckTestParameters = super.scalaCheckTestParameters.withMinSuccessfulTests(5)

  property("generated code matches") {
    code().foldLeft(true: Prop) { case (acc, (file, code)) =>
      acc && unitToProp(assertEquals(code, expected(file)))
    }
  }

  property("codecs decode and encode properly")(forAll { (in: DecodeEncodeType) =>
    var out = List.empty[String]
    var err = List.empty[String]
    val code = Process(
      Seq("yarn", "--silent", "ts-node", decodeEncodeScript.toString, in.asJson.noSpaces),
      BuildInfo.testsDir,
    ) ! ProcessLogger(s => out = out :+ s, s => err = err :+ s)
    ((code == 0) :| err.mkString("\n")) && (out match {
      case json :: Nil =>
        parser.decode[DecodeEncodeType](json).fold(
          e => (false :| e.toString),
          out => unitToProp(assertEquals(out, in)),
        )
      case _ =>
        (false :| s"Script did not output exactly one line\n\nstdout:\n\n${out.mkString("\n")}\n\nstderr:${err.mkString("\n")}\n\n")
    })
  })
}
