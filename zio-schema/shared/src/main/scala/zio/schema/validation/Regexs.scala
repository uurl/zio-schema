package zio.schema.validation

trait Regexs {

  val identifier: Validation[String] =
    Validation.regex((Regex.digitOrLetter | Regex.oneOf('_')).atLeast(1))

  //^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$
  lazy val email: Validation[String] = {
    val username       = Regex.letter ~ (Regex.digitOrLetter | Regex.oneOf('_', '.', '+', '-')).atLeast(0)
    val topLevelDomain = (Regex.digitOrLetter | Regex.oneOf('-')).between(2, 4)
    val domain =
      ((Regex.digitOrLetter | Regex.oneOf('-')).atLeast(1) ~
        (Regex.oneOf('.'))).atLeast(1) ~
        topLevelDomain

    Validation.regex(
      username ~
        Regex.oneOf('@') ~
        domain
    )
  }

  lazy val duration: Validation[String] = {
    val posDigit = Regex.between('1', '9')
    val integer  = Regex.digit.+
    val number   = integer ~ (Regex.oneOf('.') ~ Regex.digit.* ~ posDigit).?

    val second = number ~ Regex.oneOf('S')
    val minute = number ~ Regex.oneOf('M') ~ second.?
    val hour   = number ~ Regex.oneOf('H') ~ minute.? ~ second.?
    val time   = Regex.oneOf('T') ~ (hour | minute | second)

    val day      = number ~ Regex.oneOf('D')
    val week     = number ~ Regex.oneOf('W')
    val month    = number ~ Regex.oneOf('M') ~ day.?
    val year     = number ~ Regex.oneOf('Y') ~ month.? ~ day.?
    val date     = (day | month | year) ~ time.?
    val duration = Regex.oneOf('P') ~ (date | time | week)
    val regex    = BuiltIn(duration)

    Validation.builtInRegex(regex)
  }

  lazy val phoneNumberCh: Validation[String] = {
    val optionalSpace       = Regex.literal(" ").atMost(1)
    val twoDigits           = Regex.digit.exactly(2)
    val threeDigits         = Regex.digit.exactly(3)
    val plus                = Regex.literal("+")
    val doubleZero          = Regex.literal("00")
    val internationalPrefix = (plus | doubleZero) ~ Regex.literal("41")
    val nationalPrefix      = Regex.literal("0")
    val prefix              = (internationalPrefix | nationalPrefix)
    Validation.regex(
      prefix ~ optionalSpace ~
        twoDigits ~ optionalSpace ~
        threeDigits ~ optionalSpace ~
        twoDigits ~ optionalSpace ~
        twoDigits
    )
  }
}
