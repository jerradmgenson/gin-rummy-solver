import scala.annotation.tailrec

// == Symbol Table Definitions ==

class SymbolTable(stack: List[Map[String, SymbolDescriptor]] = List(defaultStackFrame)
):
  def addFrame() = SymbolTable(Map[String, SymbolDescriptor]() :: stack)
  def removeFrame() = SymbolTable(stack.tail)
  def get(id: String) =
    @tailrec
    def aux(
        stack: List[Map[String, SymbolDescriptor]]
    ): Either[String, SymbolDescriptor] = stack.headOption match
      case None => Left(s"No identifier matching `$id`")
      case Some(stackFrame) => stackFrame.get(id) match
        case None => aux(stack.tail)
        case Some(symbol) => Right(symbol)
    aux(stack)

  def add(symbol: SymbolDescriptor) = stack.headOption match
    case None             => Left("No stack frames.")
    case Some(stackFrame) => Right(SymbolTable((stackFrame + (symbol.id -> symbol)) :: stack.tail))

sealed trait SymbolDescriptor { def id: String }
object SymbolDescriptor:
  case class CardList(id: String, cards: List[Card]) extends SymbolDescriptor
  case class Template(id: String, cards: List[Card]) extends SymbolDescriptor
  case class Score(id: String, myScore: Int, theirScore: Int) extends SymbolDescriptor
  case class ConfigOption(id: String, value: Int) extends SymbolDescriptor
  case class Game(id: String) extends SymbolDescriptor
  case class Func(id: String, func: (Seq[SExpr], SymbolTable) => Either[String, (SymbolTable, Option[GameState])]) extends SymbolDescriptor
  case class Hand(
    id: String,
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
  ) extends SymbolDescriptor:
    def toList: List[Card] =
      val cards = List(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
      cards ++ c11.toList

object Hand:
  def fromSeq(cards: Seq[Card]) = cards.toList match
    case _ if !isUnique(cards) => Left("hand may not contain duplicate cards.")
    case List(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) =>
      Right(SymbolDescriptor.Hand("#hand#", c1, c2, c3, c4, c5, c6, c7, c8, c9, c10))
    case List(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) =>
      Right(SymbolDescriptor.Hand("#hand#", c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, Some(c11)))
    case l => Left(s"`hand` expects 10 or 11 arguments, not ${l.length}.")

case class Card(rank: Rank, suit: Suit)
enum Suit:
  case Spades, Hearts, Diamonds, Clubs

enum Rank:
  case Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen,
    King

val defaultStackFrame = Map(
  "hand"         -> SymbolDescriptor.Func("hand", funcHand),
  "discard-pile" -> SymbolDescriptor.Func("discard-pile", funcDiscardPile)
)

// == Built-in Function Definitions ==

def funcHand(sexpr: Seq[SExpr], symbols: SymbolTable) =
  for idents     <- sexprToIdent(sexpr)
      cards      <- traverseCards(idents)
      hand       <- Hand.fromSeq(cards)
      newSymbols <- symbols.add(hand)
  yield (newSymbols, None)

def funcDiscardPile(sexpr: Seq[SExpr], symbols: SymbolTable) =
  for _          <- Either.cond(sexpr.length >= 1, (), "`discard-pile` expects at least 1 argument.")
      idents     <- sexprToIdent(sexpr)
      cards      <- traverseCards(idents)
      discards   <- Either.cond(isUnique(cards), cards, s"discard-pile contains duplicate cards: $cards")
      newSymbols <- symbols.add(SymbolDescriptor.CardList("#discard-pile#", discards.toList))
  yield (newSymbols, None)

// == Helper Functions ==

def isUnique[T](s: Seq[T]) = s.length == s.distinct.length

def sexprToIdent(sexpr: Seq[SExpr]) = sexpr match
  case s: Seq[_] if s.forall(_.isInstanceOf[SExpr.Ident]) => Right(s.asInstanceOf[Seq[SExpr.Ident]])
  case _ => Left("Type error: expected a sequence of identifiers")

def traverseCards(idents: Seq[SExpr.Ident]) =
  idents.foldLeft[Either[String, Seq[Card]]](Right(Seq.empty)) { (accEither, ident) =>
    for acc  <- accEither
      card <- identToCard(ident)
    yield acc :+ card
  }

def identToCard(ident: SExpr.Ident) =
  for (r, s) <- Either.cond(ident.name.length == 2, (ident.name(0), ident.name(1)), s"No valid Card can be inferred from $ident")
    rank   <- charToRank(r)
    suit   <- charToSuit(s)
  yield Card(rank, suit)

def charToRank(r: Char) = r match
  case 'a' => Right(Rank.Ace)
  case '2' => Right(Rank.Two)
  case '3' => Right(Rank.Three)
  case '4' => Right(Rank.Four)
  case '5' => Right(Rank.Five)
  case '6' => Right(Rank.Six)
  case '7' => Right(Rank.Seven)
  case '8' => Right(Rank.Eight)
  case '9' => Right(Rank.Nine)
  case 't' => Right(Rank.Ten)
  case 'j' => Right(Rank.Jack)
  case 'q' => Right(Rank.Queen)
  case 'k' => Right(Rank.King)
  case _   => Left(s"`$r` is not a valid rank.")

def charToSuit(s: Char) = s match
  case 's' => Right(Suit.Spades)
  case 'c' => Right(Suit.Clubs)
  case 'd' => Right(Suit.Diamonds)
  case 'h' => Right(Suit.Hearts)
  case _   => Left(s"`$s` is not a valid suit.")
