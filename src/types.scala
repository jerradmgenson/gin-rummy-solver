enum GRLType:
  case Card, Wildcard, Template, Integer, GameState, Function

enum CompilerError:
  case SyntaxError(desc: String)
  case ArityError(funcName: String, expected: Seq[Int], found: Int)
  case TypeError(expected: Seq[GRLType], found: Option[GRLType])
  case ValueError(desc: String)
  case UndefinedError(symbolName: String)
  case RedefinitionError(symbolName: String)
  case InternalError(desc: String)

  def description: String = this match
    case SyntaxError(desc) => desc
    case ArityError(funcName, expected, found) =>
      val expectedStr = expected.mkString("[", ", ", "]")
      s"Incorrect number of arguments to $funcName. Expected: $expectedStr Found: $found"
    case TypeError(expected, found) =>
      val expectedStr = expected.mkString("[", ", ", "]")
      found match
        case Some(f) => s"Incorrect type. Expected: $expectedStr Found: $f"
        case None    => s"Incorrect type. Expected: $expectedStr"
    case ValueError(desc) => desc
    case UndefinedError(symbolName) => s"`$symbolName` is not defined."
    case RedefinitionError(symbolName) => s"Can not redefine built-in name `$symbolName`."
    case InternalError(desc) => desc
