import scala.annotation.tailrec

@tailrec
def evaluateProgram(
  program: Seq[SExpr],
  symbols: SymbolTable = SymbolTable(),
  gameStates: List[GameState] = List.empty
): Either[String, List[GameState]] = program match
  case Nil                        => Right(gameStates)
  case SExpr.SList(sexpr) +: tail =>
    evaluateSList(sexpr, symbols) match
      case Right(newSymbols, newGameState) => evaluateProgram(tail, newSymbols, gameStates ++ newGameState)
      case Left(s) => Left(s)
  case _ => Left(s"Expected S-expression list but found ${program.head}")

def evaluateSList(
  sexpr: Seq[SExpr],
  symbols: SymbolTable
): Either[String, (SymbolTable, Option[GameState])] =
  println(s"S-expression list: $sexpr")
  sexpr match
    case SExpr.Ident(name) +: tail => evaluateCall(name, tail, symbols)
    case head +: _                 => Left(s"Function calls must begin with an identifier, not $head")

def evaluateCall(
  funcName: String,
  args: Seq[SExpr],
  symbols: SymbolTable
): Either[String, (SymbolTable, Option[GameState])] =
  for symbol <- symbols.get(funcName)
      func   <- symbol match
        case SymbolDescriptor.Func(_, func) => Right(func)
        case _                              => Left(s"$funcName is not a function.")
      (newSymbols, newGameState) <- func(args, symbols)
  yield (newSymbols, newGameState)

case class GameState(
  hand: SymbolDescriptor.Hand,
  discardPile: List[Card],
  currentScore: (Int, Int),
  endScore: Int,
  gin: Int,
  bigGin: Int,
  undercut: Int,
  knockThreshold: Int,
  remainingStock: Option[Int] = None
)
