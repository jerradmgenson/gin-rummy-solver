import scala.annotation.tailrec

class SymbolTable(
    stack: List[Map[String, SymbolDescriptor]] = List(
      Map[String, SymbolDescriptor]()
    )
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
