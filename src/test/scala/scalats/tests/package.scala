package scalats
package tests

import io.circe.derivation.{Configuration => CirceConfig}
import java.io.File
import java.nio.file.Files
import scala.io.Source
import scala.util.Using

extension(file: File) {
  final def /(name: String): File = new File(file, name)
  final def read: String = Using.resource(Source.fromFile(file))(_.mkString)
}

val outputDir = testsDir / "output"

given customType: TsCustomType = TsCustomType.none
given customOrd: TsCustomOrd = TsCustomOrd.none
given imports: TsImports.Available = TsImports.Available(TsImports.Config())

given circeConfig: CirceConfig = CirceConfig(discriminator = Some("_tag"))

def writeDecodeEncodeScript(scriptFile: File, codecName: String, codecValue: String, codecFile: File): Unit = {
  Files.write(scriptFile.toPath, s"""
import * as E from "fp-ts/lib/Either";
import { pipe } from "fp-ts/lib/function";
import * as t from "io-ts";
import { $codecName } from "${codecFile.toString.stripSuffix(".ts")}";

pipe(
  process.argv[2],
  s => JSON.parse(s),
  $codecValue.decode,
  E.fold(
    errs => {
      console.error(JSON.stringify(errs, null, 2));
      process.exit(1);
    },
    x => {
      console.log(JSON.stringify($codecValue.encode(x)));
      process.exit(0);
    },
  ),
);
""".getBytes("UTF-8"))

    ()
  }
