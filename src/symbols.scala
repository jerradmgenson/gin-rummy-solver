import scala.annotation.tailrec

class SymbolTable(stack: List[Map[String, SymbolDescriptor]] = List(defaultStackFrame)
):
  def addFrame() = SymbolTable(Map[String, SymbolDescriptor]() :: stack)
  def removeFrame() = SymbolTable(stack.tail)
  def get(name: String) =
    @tailrec
    def aux(
        stack: List[Map[String, SymbolDescriptor]]
    ): Option[SymbolDescriptor] = stack.headOption match
      case None             => None
      case Some(stackFrame) => stackFrame.get(name) match
        case None         => aux(stack.tail)
        case Some(symbol) => Some(symbol)
    aux(stack)

  def set(name: String, symbol: SymbolDescriptor) = stack.headOption match
    case None             => None
    case Some(stackFrame) => Some((stackFrame + (name -> symbol)) :: stack.tail)

sealed trait SymbolDescriptor { def id: String }
object SymbolDescriptor:
  case class CardList(
      id: String,
      cards: List[Card],
      wildcards: List[SExpr.Wildcard],
      templates: List[String]
  ) extends SymbolDescriptor
  case class Template(id: String, cards: List[Card]) extends SymbolDescriptor
  case class Score(id: String, myScore: Int, theirScore: Int) extends SymbolDescriptor
  case class ConfigOption(id: String, value: Int) extends SymbolDescriptor
  case class Game(id: String) extends SymbolDescriptor
  case class Func(id: String, func: (Seq[SExpr], SymbolTable) => Either[String, (SymbolTable, Option[GameState])]) extends SymbolDescriptor

case class Card(rank: Rank, suit: Suit)
enum Suit:
  case Spades, Hearts, Diamonds, Clubs

enum Rank:
  case Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen,
    King

val defaultStackFrame = Map(
  "hand" -> SymbolDescriptor.Func("hand", funcHand)
)

def funcHand(sexpr: Seq[SExpr], symbols: SymbolTable) =
  for _ <- Either.cond(sexpr.length == 10 || sexpr.length == 11, (), "Incorrect number of arguments to `hand`.")
      cards <- traverseCards(sexpr)
  yield (symbols, None)

def traverseCards(sexprs: Seq[SExpr]) =
  sexprs.foldLeft[Either[String, Seq[Card]]](Right(Seq.empty)) { (accEither, expr) =>
    for acc  <- accEither
        card <- identToCard(expr)
    yield acc :+ card
  }

def identToCard(sexpr: SExpr) =
  for (r, s) <- sexpr match { case SExpr.Ident(s"$r$s") => Right((r, s)) case _ => Left(s"No valid Card can be inferred from $sexpr") }
      rank   <- strToRank(r)
      suit   <- strToSuit(s)
  yield Card(rank, suit)

def strToRank(r: String) = r match
  case "a" => Right(Rank.Ace)
  case "2" => Right(Rank.Two)
  case "3" => Right(Rank.Three)
  case "4" => Right(Rank.Four)
  case "5" => Right(Rank.Five)
  case "6" => Right(Rank.Six)
  case "7" => Right(Rank.Seven)
  case "8" => Right(Rank.Eight)
  case "9" => Right(Rank.Nine)
  case "t" => Right(Rank.Ten)
  case "j" => Right(Rank.Jack)
  case "q" => Right(Rank.Queen)
  case "k" => Right(Rank.King)
  case _   => Left(s"$r is not a known rank.")

def strToSuit(s: String) = s match
  case "s" => Right(Suit.Spades)
  case "c" => Right(Suit.Clubs)
  case "d" => Right(Suit.Diamonds)
  case "h" => Right(Suit.Hearts)
  case _   => Left(s"$s is not a known suit.")
