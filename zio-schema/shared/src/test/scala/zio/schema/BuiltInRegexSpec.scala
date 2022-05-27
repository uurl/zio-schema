package zio.schema.validation

import zio.test._

object BuiltInRegexSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("Compiling Regex")(
    test("compile oneOf singleton") {
      val regex    = Regex.oneOf('a')
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "a")
    },
    test("compile oneOf several elements") {
      val regex    = Regex.oneOf('a', 'b', 'c')
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "[abc]")
    },
    test("compile *") {
      val regex    = Regex.oneOf('a', 'b', 'c').*
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "([abc])*")
    },
    test("compile +") {
      val regex    = Regex.oneOf('a', 'b', 'c').+
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "([abc])+")
    },
    test("compile ?") {
      val regex    = Regex.oneOf('a', 'b', 'c').?
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "([abc])?")
    },
    test("compile repeat n") {
      val regex    = Regex.oneOf('a', 'b', 'c').between(2, 2)
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "([abc]){2}")
    },
    test("compile repeat n+") {
      val regex    = Regex.oneOf('a', 'b', 'c').atLeast(2)
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "([abc]){2,}")
    },
    test("compile repeat n to m") {
      val regex    = Regex.oneOf('a', 'b', 'c').between(2, 4)
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "([abc]){2,4}")
    },
    test("compile alternative") {
      val regex    = Regex.oneOf('a', 'b', 'c') | Regex.oneOf('d', 'e', 'f')
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "([abc])|([def])")
    },
    test("compile letter") {
      val regex    = Regex.letter
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "[a-zA-Z]")
    },
    test("compile digit") {
      val regex    = Regex.digit
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "\\d")
    },
    test("compile sequence") {
      val regex    = Regex.oneOf('a', 'b', 'c') ~ Regex.oneOf('d', 'e', 'f')
      val regexStr = BuiltIn.toRegexString(regex)
      assertTrue(regexStr == "[abc][def]")
    }
  )
}
