import scala.annotation.tailrec

// == Symbol Table Definitions ==
// ******************************

type Symbol = SymbolDescriptor | Vector[SymbolDescriptor]
type SymbolStack = List[Map[String, Symbol]]

class SymbolTable(stack: SymbolStack = List(defaultStackFrame)
):
  def addFrame() = SymbolTable(Map[String, Vector[SymbolDescriptor]]() :: stack)
  def removeFrame() = SymbolTable(stack.tail)
  def get(id: String) =
    @tailrec
    def aux(
        stack: SymbolStack
    ): Either[CompilerError.UndefinedError, Symbol] = stack.headOption match
      case None             => Left(CompilerError.UndefinedError(id))
      case Some(stackFrame) => stackFrame.get(id) match
        case None         => aux(stack.tail)
        case Some(symbol) => Right(symbol)
    val symbol = aux(stack)
    println(s"Retrieved symbol: $symbol")
    symbol

  def add(symbol: SymbolDescriptor): Either[CompilerError, SymbolTable] = stack.headOption match
    case None             => Left(CompilerError.InternalError("No stack frames."))
    case Some(stackFrame) => stackFrame.get(symbol.id) match
      case Some(oldSymbol: SymbolDescriptor) if builtInNames.contains(oldSymbol.id) =>
        Left(CompilerError.RedefinitionError(symbol.id))
      case _ =>
        println(s"Added symbol: $symbol")
        Right(SymbolTable(stackFrame + (symbol.id -> symbol) :: stack.tail))

  def add(symbols: Seq[SymbolDescriptor]): Either[CompilerError, SymbolTable] =
    for stackFrame <- stack.headOption.toRight(CompilerError.InternalError("No stack frames."))
        symbolID   <- Either.cond(symbols.length >= 1, symbols(0).id, CompilerError.InternalError("symbols must have length >= 1."))
        _          <- Either.cond(symbols.forall(_.id == symbolID), (), CompilerError.InternalError("All symbols must have the same id."))
        oldSymbol  = stackFrame.get(symbolID) match { case Some(s: SymbolDescriptor) => Some(s) case _ => None }
        _          <- Either.cond(
          oldSymbol.map(s => !builtInNames.contains(s.id)).forall(identity),
          (),
          CompilerError.RedefinitionError(symbolID)
        )
        _          = println(s"Added symbols:\n${symbols.mkString("\n")}")
    yield SymbolTable(stackFrame + (symbolID -> symbols.toVector) :: stack.tail)

sealed trait SymbolDescriptor { def id: String }
object SymbolDescriptor:
  case class CardList(id: String, cards: Seq[Card]) extends SymbolDescriptor
  object CardList:
    def apply(
      id: String,
      cards: Seq[Card],
      allowDuplicates: Boolean = false,
      minCards: Int = 1
    ): Either[CompilerError, CardList] =
      if !allowDuplicates && !isUnique(cards)
      then Left(CompilerError.ValueError(s"`$id` may not contain duplicate cards."))
      else if cards.length < minCards
      then Left(CompilerError.ArityError(id, Seq(minCards), cards.length))
      else Right(CardList(id, cards))

  case class Score(id: String, myScore: Int, theirScore: Int) extends SymbolDescriptor
  case class ConfigOption(id: String, value: Int) extends SymbolDescriptor
  case class Game(id: String) extends SymbolDescriptor
  case class Func private (id: String, func: (Seq[SExpr], SymbolTable) => Either[CompilerError, (SymbolTable, Option[GameState])]) extends SymbolDescriptor
  object Func:
    def apply(
      name: String,
      func: (Seq[SExpr], SymbolTable) => Either[CompilerError, (SymbolTable, Option[GameState])],
      nargs: Int
    ): Func =
      val arityCheckWrapper = (sexpr: Seq[SExpr], symbols: SymbolTable) =>
        if sexpr.length == nargs
        then func(sexpr, symbols)
        else Left(CompilerError.ArityError(name, Seq(nargs), sexpr.length))

      Func(name, arityCheckWrapper)

    def apply(
      name: String,
      func: (Seq[SExpr], SymbolTable) => Either[CompilerError, (SymbolTable, Option[GameState])],
      minArgs: Int,
      maxArgs: Option[Int]
    ): Func =
      val arityCheckWrapper = (sexpr: Seq[SExpr], symbols: SymbolTable) => maxArgs match
        case Some(upper) =>
          val l = sexpr.length
          if l >= minArgs & l <= upper
          then func(sexpr, symbols)
          else Left(CompilerError.ArityError(name, Seq(minArgs, upper), l))
        case None =>
          if sexpr.length >= minArgs
          then func(sexpr, symbols)
          else Left(CompilerError.ArityError(name, Seq(minArgs), sexpr.length))

      Func(name, arityCheckWrapper)

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
    def apply(cards: Seq[Card]): Either[CompilerError, Hand] = cards.toList match
      case _ if !isUnique(cards) => Left(CompilerError.ValueError("`hand` may not contain duplicate cards."))
      case List(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) =>
        Right(Hand("#hand#", c1, c2, c3, c4, c5, c6, c7, c8, c9, c10))
      case List(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) =>
        Right(Hand("#hand#", c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, Some(c11)))
      case _ => Left(CompilerError.ArityError("hand", Seq(10, 11), cards.length))

case class Card(rank: Rank, suit: Suit)
object Card:
  def apply(ident: SExpr.Ident): Either[CompilerError, Card] =
    for (r, s) <- Either.cond(
                    ident.name.length == 2,
                    (ident.name(0), ident.name(1)),
                    CompilerError.ValueError(s"No valid Card can be inferred from $ident"))
         rank  <- Rank.fromChar(r)
         suit  <- Suit.fromChar(s)
    yield Card(rank, suit)

enum Suit:
  case Spades, Hearts, Diamonds, Clubs

object Suit:
  def fromChar(s: Char) = suitMap.get(s).toRight(CompilerError.ValueError(s"Invalid suit: $s"))

enum Rank:
  case Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen,
    King

object Rank:
  def fromChar(r: Char) = rankMap.get(r).toRight(CompilerError.ValueError(s"Invalid rank: $r"))

val rankMap = Map(
  'a' -> Rank.Ace,
  '2' -> Rank.Two,
  '3' -> Rank.Three,
  '4' -> Rank.Four,
  '5' -> Rank.Five,
  '6' -> Rank.Six,
  '7' -> Rank.Seven,
  '8' -> Rank.Eight,
  '9' -> Rank.Nine,
  't' -> Rank.Ten,
  'j' -> Rank.Jack,
  'q' -> Rank.Queen,
  'k' -> Rank.King
)

val suitMap = Map(
  's' -> Suit.Spades,
  'c' -> Suit.Clubs,
  'd' -> Suit.Diamonds,
  'h' -> Suit.Hearts
)

val configOptions = Seq(
  ("end-score", Some(100)),
  ("gin", Some(20)),
  ("big-gin", Some(20)),
  ("undercut", Some(10)),
  ("knock-threshold", Some(10)),
  ("remaining-stock", None),
)

val configOptionFuncs = configOptions.map((name, _) => (name, configOption(name)))
val configOptionDefaults = configOptions.collect {
  case (name, Some(v)) => (s"#$name#", SymbolDescriptor.ConfigOption(s"#$name#", v))
}

val builtInFuncs = Seq(
  SymbolDescriptor.Func("hand", funcHand, 10, Some(11)),
  SymbolDescriptor.Func("discard-pile", funcDiscardPile, 1, None),
  SymbolDescriptor.Func("let", funcLet, 2, None),
  SymbolDescriptor.Func("score", funcScore, 2),
).map(symbol => (symbol.id, symbol)).toMap

val defaultStackFrame = builtInFuncs ++ configOptionFuncs ++ configOptionDefaults
val builtInNames = (builtInFuncs ++ configOptionFuncs).keySet

// == Built-in Function Definitions ==
// ***********************************

def funcHand(sexpr: Seq[SExpr], symbols: SymbolTable) =
  for cards       <- expandCardMacros(sexpr, symbols)
      hands       <- traverse[Seq[Card], SymbolDescriptor.Hand](SymbolDescriptor.Hand(_), cards)
      newSymbols  <- symbols.add(hands)
  yield (newSymbols, None)

def funcDiscardPile(sexpr: Seq[SExpr], symbols: SymbolTable) =
  for cards       <- expandCardMacros(sexpr, symbols)
      discardSyms <- traverse[Seq[Card], SymbolDescriptor.CardList](SymbolDescriptor.CardList("#discard-pile#", _), cards)
      newSymbols  <- symbols.add(discardSyms)
  yield (newSymbols, None)

def funcLet(sexpr: Seq[SExpr], symbols: SymbolTable) =
  for idents     <- traverse[SExpr, SExpr.Ident](
                      _ match { case i: SExpr.Ident => Right(i) case _ => Left(CompilerError.SyntaxError("Arguments to `let` must be identifiers."))},
                      sexpr)
      id         =  idents.head.name
      cards      <- traverse[SExpr.Ident, Card](Card.apply(_), idents.tail)
      _          <- Either.cond(
                      isUnique(cards),
                      (),
                      CompilerError.ValueError(s"`let` contains duplicate cards: $cards"))
      newSymbols <- symbols.add(SymbolDescriptor.CardList(id, cards))
  yield (newSymbols, None)

def funcScore(sexpr: Seq[SExpr], symbols: SymbolTable) = sexpr match
  case Seq(SExpr.Number(myScore), SExpr.Number(theirScore)) =>
    symbols.add(SymbolDescriptor.Score("#score#", myScore, theirScore)) match
      case Right(newSymbols)   => Right((newSymbols, None))
      case Left(compilerError) => Left(compilerError)
  case _ => Left(CompilerError.TypeError(Seq(GRLType.Integer), None))

def configOption(optionName: String) =
  val configFunc = (sexpr: Seq[SExpr], symbols: SymbolTable) => sexpr match
    case Seq(SExpr.Number(endScore)) => symbols.add(SymbolDescriptor.ConfigOption(s"#$optionName#", endScore)) match
      case Right(newSymbols)   => Right((newSymbols, None))
      case Left(compilerError) => Left(compilerError)
    case _ => Left(CompilerError.TypeError(Seq(GRLType.Integer), None))
  SymbolDescriptor.Func(optionName, configFunc, 1)

// == Helper Functions ==
// **********************

def isUnique[T](s: Seq[T]) = s.length == s.distinct.length

def traverse[T, U](decode: T => Either[CompilerError, U], idents: Seq[T]): Either[CompilerError, Seq[U]] =
  idents.foldLeft[Either[CompilerError, Seq[U]]](Right(Seq.empty)) { (accEither, ident) =>
    for acc          <- accEither
        decodedValue <- decode(ident)
    yield decodedValue +: acc
  }
  .map(_.reverse)

def expandWildcard(wildcard: SExpr.Wildcard): Either[CompilerError, Seq[Card]]= wildcard match
  case SExpr.Wildcard(None)            => Right(genCards())
  case SExpr.Wildcard(Some(invariant)) =>
    val rank = Rank.fromChar(invariant)
    val suit = Suit.fromChar(invariant)
    (rank, suit) match
      case (Right(r), Left(_)) => Right(genCards(r))
      case (Left(_), Right(s)) => Right(genCards(s))
      case _                   => Left(CompilerError.ValueError(s"Invalid invariant: $invariant"))

def genCards(rank: Rank) = suitMap.values.map(Card(rank, _)).toSeq
def genCards(suit: Suit) = rankMap.values.map(Card(_, suit)).toSeq
def genCards() = {
  for rank <- rankMap.values
      suit <- suitMap.values
  yield Card(rank, suit)
}.toSeq


def expandLet(baseCards: Seq[Card], letCards: Seq[Card]) =
  letCards.view.map(_ +: baseCards).filter(isUnique).toSeq

def expandCardMacros(sexpr: Seq[SExpr], symbols: SymbolTable): Either[CompilerError, Seq[Seq[Card]]] =
  val idents    = sexpr.collect { case i: SExpr.Ident => i }
  val wildcards = sexpr.collect { case w: SExpr.Wildcard => w }
  val baseCards = idents.collect { Card.apply(_) match { case Right(c) => c }}
  val letIds    = idents.filter(Card.apply(_).isLeft)
  for _             <- Either.cond(
                         baseCards.length + wildcards.length + letIds.length == sexpr.length,
                         (),
                         CompilerError.TypeError(Seq(GRLType.Card, GRLType.Wildcard, GRLType.Template), None))
      letCards      <- traverse[SExpr.Ident, Seq[Card]](i =>
                         for s <- symbols.get(i.name)
                             c <- s match
                                      case SymbolDescriptor.CardList(_, c) => Right(c)
                                      case _ => Left(CompilerError.TypeError(Seq(GRLType.Card), None))
                         yield c,
                         letIds)
      wildcardExpanded <- traverse[SExpr.Wildcard, Seq[Card]](expandWildcard, wildcards)
      fullyExpanded = genCardCombinations(baseCards, letCards ++ wildcardExpanded).view.filter(isUnique).map(_.toSet).toSet
  yield fullyExpanded.map(_.toSeq).toSeq

def genCardCombinations(baseCards: Seq[Card], varCards: Seq[Seq[Card]]) =
  val varCombinations = varCards.foldLeft(Seq(Seq.empty[Card])) { (acc, cards) =>
    for combo <- acc
        card  <- cards
    yield combo :+ card
  }

  varCombinations.map(baseCards ++ _)
