package zio.schema.validation

import zio.schema.validation.Regex.{ Alternate, CharacterSet, Empty, Repeat }

sealed trait Regex {
  def atLeast(n: Int): Regex             = Regex.Repeat(this, Some(n), None)
  def atMost(n: Int): Regex              = Regex.Repeat(this, None, Some(n))
  def between(min: Int, max: Int): Regex = Regex.Repeat(this, Some(min), Some(max))
  def exactly(n: Int): Regex             = Regex.Repeat(this, Some(n), Some(n))
  def + : Regex                          = atLeast(1)
  def * : Regex                          = atLeast(0)
  def ? : Regex                          = atMost(1)
  def ~(that: Regex): Regex              = Regex.Sequence(this, that)
  def |(that: Regex): Regex              = Regex.Alternate(this, that)

  def test(string: String): Boolean = {
    def loop(regex: Regex, n: Int): Int =
      regex match {
        case CharacterSet(set) =>
          if (n >= string.length) -1
          else {
            val char = string.charAt(n)
            if (set.contains(char)) n + 1
            else -1
          }
        case Repeat(regex, min0, max0) =>
          val min            = min0.getOrElse(0)
          val max            = max0.getOrElse(Int.MaxValue)
          var matchCount     = 0
          var n2             = n
          var lastValidIndex = n2
          while (matchCount < max && n2 >= 0) {
            n2 = loop(regex, n2)
            if (n2 >= 0) {
              matchCount = matchCount + 1
              lastValidIndex = n2
            }
          }
          if (matchCount >= min && matchCount <= max) lastValidIndex
          else -1
        case Empty => n
        case Alternate(left, right) =>
          val n2 = loop(left, n)
          if (n2 >= 0) n2
          else loop(right, n)
        case Regex.Letter =>
          if (n >= string.length) -1
          else {
            val char = string.charAt(n)
            if (char.isLetter) n + 1
            else -1
          }
        case Regex.Digit =>
          if (n >= string.length) -1
          else {
            val char = string.charAt(n)
            if (char.isDigit) n + 1
            else -1
          }
        case Regex.Sequence(first, second) =>
          val n2 = loop(first, n)
          if (n2 < 0) -1
          else loop(second, n2)
      }

    loop(this, 0) == string.length()
  }
}

object Regex {
  final case class CharacterSet(set: Set[Char]) extends Regex

  final case class Repeat(regex: Regex, min: Option[Int], max: Option[Int]) extends Regex

  final case class Sequence(first: Regex, second: Regex) extends Regex

  case object Letter extends Regex

  case object Digit extends Regex

  case object Empty extends Regex

  final case class Alternate(left: Regex, right: Regex) extends Regex

  def between(minChar: Char, maxChar: Char): Regex = CharacterSet((minChar to maxChar).toSet)

  def filter(f: Char => Boolean): Regex = CharacterSet((Char.MinValue to Char.MaxValue).filter(f).toSet)

  def literal(str: String): Regex = {
    def loop(l: List[Char], acc: Regex): Regex =
      l.toList match {
        case head :: Nil  => acc ~ CharacterSet(Set(head))
        case head :: tail => loop(tail, acc ~ CharacterSet(Set(head)))
        case Nil          => acc
      }

    loop(str.toList, Empty)
  }

  def oneOf(chars: Char*): Regex = CharacterSet(chars.toSet)

  val digit: Regex = Digit

  val letter: Regex = Letter

  val digitOrLetter: Regex = digit | letter
}

case class BuiltIn(regex: Regex) { self =>
  private val regexString   = BuiltIn.toRegexString(regex)
  private val compiledRegex = regexString.r

  def test(string: String): Boolean = compiledRegex.matches(string)
}

/**
 * Produce a regex string from the encoding and use scala's native regex matcher
 */
object BuiltIn {

  private def escapeChar(ch: Char): String =
    ch match {
      case '.'  => "\\."
      case '^'  => "\\^"
      case '$'  => "\\$"
      case '|'  => "\\|"
      case '*'  => "\\*"
      case '+'  => "\\+"
      case '?'  => "\\?"
      case '('  => "\\("
      case ')'  => "\\)"
      case '['  => "\\["
      case ']'  => "\\]"
      case '{'  => "\\{"
      case '}'  => "\\}"
      case '\\' => "\\\\"
      case _    => ch.toString
    }

  def toRegexString(regex: Regex): String = {
    def loop(regex: Regex): String =
      regex match {
        case CharacterSet(elem) if elem.size == 1 =>
          escapeChar(elem.head)
        case CharacterSet(set) =>
          set.toList.sorted.map(escapeChar).mkString("[", "", "]")
        case Repeat(regex, Some(0), None) =>
          s"(${loop(regex)})*"
        case Repeat(regex, Some(1), None) =>
          s"(${loop(regex)})+"
        case Repeat(regex, None, Some(1)) =>
          s"(${loop(regex)})?"
        case Repeat(regex, Some(n), Some(m)) if n == m =>
          s"(${loop(regex)}){$n}"
        case Repeat(regex, Some(n), None) =>
          s"(${loop(regex)}){$n,}"
        case Repeat(regex, None, Some(m)) =>
          s"(${loop(regex)}){0,$m}"
        case Repeat(regex, Some(n), Some(m)) =>
          s"(${loop(regex)}){$n,$m}"
        case Repeat(_, None, None) =>
          throw new IllegalArgumentException("Cannot have no repeat count")
        case Empty => ""
        case Alternate(left, right) =>
          s"(${loop(left)})|(${loop(right)})"
        case Regex.Letter =>
          "[a-zA-Z]"
        case Regex.Digit =>
          "\\d"
        case Regex.Sequence(first, second) =>
          s"${loop(first)}${loop(second)}"
      }
    loop(regex)
  }
}
