enum GRLType:
  case Card, Wildcard, Template, Integer, GameState, Function

sealed trait CompilerError:
  def description: String

object CompilerError:
  case class SyntaxError(description: String) extends CompilerError
  case class ArityError(funcName: String, expected: Seq[Int], found: Int) extends CompilerError:
    private val expectedStr = expected.mkString("[", ", ", "]")
    val description = s"Incorrect number of arguments to $funcName. Expected: $expected Found: $found"
  case class TypeError(expected: Seq[GRLType], found: Option[GRLType]) extends CompilerError:
    private val expectedStr = expected.mkString("[", ", ", "]")
    val description = found match
      case Some(f) => s"Incorrect type. Expected: $expectedStr Found: $found"
      case None    => s"Incorrect type. Expected: $expectedStr"
  case class ValueError(description: String) extends CompilerError
  case class UndefinedError(symbolName: String) extends CompilerError:
    val description = s"Name `$symbolName` is not defined."
  case class RedefinitionError(symbolName: String) extends CompilerError:
    val description = s"Name `$symbolName` is already defined."
  case class KeywordError(keyword: String) extends CompilerError:
    val description = s"Keyword `$keyword` can not be used as an identifier name."
  case class InternalError(description: String) extends CompilerError
