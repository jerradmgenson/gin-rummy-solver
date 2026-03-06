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
  for symbol <- symbols.get(funcName).toRight(s"Function `$funcName` does not exist.")
      func <- symbol match
        case SymbolDescriptor.Func(_, func) => Right(func)
        case _                              => Left(s"$funcName is not a function.")
      (newSymbols, newGameState) <- func(args, symbols)
  yield (newSymbols, newGameState)

case class GameState(
  hand: Hand,
  discardPile: List[Card],
  currentScore: (Int, Int),
  endScore: Int,
  gin: Int,
  bigGin: Int,
  undercut: Int,
  knockThreshold: Int,
  remainingStock: Option[Int] = None
)

case class Hand(
  c1: Card,
  c2: Card,
  c3: Card,
  c4: Card,
  c5: Card,
  c6: Card,
  c7: Card,
  c8: Card,
  c9: Card,
  c10: Card,
  c11: Option[Card] = None
):
  def toList: List[Card] =
    val cards = List(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
    cards ++ c11.toList
