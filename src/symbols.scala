/*
 * Contains implementations of SymbolTable, SymbolDescriptor, built-in functions,
 * and supporting classes, objects, and methods for Gin Rummy Language (GRL).
 *
 * Copyright 2026 Jerrad Michael Genson
 * License: https://github.com/jerradmgenson/gin-rummy-solver/blob/main/LICENSE
 */

import scala.annotation.tailrec

// ******************************
// == Symbol Table Definitions ==
// ******************************

/** Represents the type of objects stored in the SymbolTable. */
type Symbol = SymbolDescriptor | Vector[SymbolDescriptor]

/** The main data structure used internally by the SymbolTable. */
type SymbolStack = List[Map[String, Symbol]]

/**
  * A symbol table implementation for managing variables, function definitions,
  * and scope in GRL.
  *
  * Implemented as a stack of stack frames, where each stack frame contains the
  * symbols defined in a certain scope in a GRL program. A complete description
  * of these symbols is given by the SymbolDescriptor sealed trait and its
  * case class.
  *
  * @param stack A SymbolStack to initialize the SymbolTable with.
  *   This parameter is usually omitted when instantiating a SymbolTable.
  */
class SymbolTable(stack: SymbolStack = List(defaultStackFrame)):

  /**
    * Push a new stack frame onto the SymbolTable.
    *
    * A new stack frame should be created anytime a GRL program enters a new
    * nested scope. When the GRL program exits the nested scope, the stack frame
    * should be removed by calling SymbolTable.delFrame().
    *
    * @return A new SymbolTable object containing an additional stack frame.
    */
  def addFrame() = SymbolTable(Map[String, Vector[SymbolDescriptor]]() :: stack)

  /**
    * Pop the stack frame at the top of SymbolTable off of the stack.
    *
    * @return Either a new SymbolTable object without the stack frame (Right)
    *   or a CompilerError (Left).
    */
  def delFrame() = stack match
    case _ :: tail => Right(SymbolTable(tail))
    case _ => Left(CompilerError.InternalError("SymbolTable contains no stack frames to delete."))

  /**
    * Get a symbol from the SymbolTable.
    *
    * @param id The id attribute of the symbol to retrieve.
    * @return Either the symbol (or vector of symbols) matching id (Right) or
    *   a CompilerError (Left).
    */
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

  /**
    * Add a single symbol to the SymbolTable.
    *
    * @param symbol The symbol to add.
    * @return Either a new SymbolTable with the added symbol (Right) or a
    *   CompilerError (Left).
    */
  def add(symbol: SymbolDescriptor): Either[CompilerError, SymbolTable] = stack.headOption match
    case None             => Left(CompilerError.InternalError("No stack frames."))
    case Some(stackFrame) => stackFrame.get(symbol.id) match
      case Some(oldSymbol: SymbolDescriptor) if builtInNames.contains(oldSymbol.id) =>
        Left(CompilerError.RedefinitionError(symbol.id))
      case _ =>
        println(s"Added symbol: $symbol")
        Right(SymbolTable(stackFrame + (symbol.id -> symbol) :: stack.tail))

  /**
    * Add a sequence of symbols to the SymbolTable.
    *
    * All symbols must have the same id and be of the same time. When
    * SymbolTable.get is called with this id, it will return all associated
    * symbols as a Vector.
    *
    * @param symbols The sequence of symbols to add to the SymbolTable.
    * @return Either a new SymbolTable with the added symbols (Right) or a
    *   CompilerError (Left).
    */
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

/** Represents an object that can be managed by the SymbolTable. */
sealed trait SymbolDescriptor { def id: String }

/** Namespace for the SymbolDescriptor variants. */
object SymbolDescriptor:

  /**
    * Represents a general list of cards (such as a discard pile).
    *
    * @param id The symbol's name/id.
    * @param cards Individual cards in the CardList.
    * @param allowDuplicates Whether or not duplicate cards are allowed in
    *  this CardList (default: false).
    * @param minCards Minimum number of cards this CardList is allowed to have
    *   (default: 1).
    * @return Either a CardList (Right) or a CompilerError (Left).
    */
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

  /**
    * Represents the game's current score.
    * @param myScore The user's current score.
    * @param theirScore The user's opponent's score.
    */
  case class Score(id: String, myScore: Int, theirScore: Int) extends SymbolDescriptor
  object Score:
    val id = "#score#"
    def apply(myScore: Int, theirScore: Int): Score = Score(id, myScore, theirScore)

  /**
    * Represents a configuration option for a Gin Rummy game.
    *
    * Some examples: knock threshold, end score, Gin value, etc.
    *
    * @param id The name/id of the ConfigOption.
    * @param value The value of the ConfigOption.
    */
  case class ConfigOption(id: String, value: Int) extends SymbolDescriptor

  /**
    * Represents a unique, abstract game state to be expanded and then
    * evaluated by the solver.
    *
    * @param id The name/id of the Game.
    */
  case class Game(id: String) extends SymbolDescriptor

  /**
    * Represents a built-in GRL function.
    *
    * This SymbolDescriptor encodes the actual implementation of a GRL function.
    * SymbolDescriptor.Func wraps the given func with arity-checking logic
    * that runs everytime func is called.
    *
    * @param name The name/id of the Func.
    * @param func A Scala function that implements the corresponding GRL function.
    *   The function accepts a Seq of SExprs from the AST and a SymbolTable and
    *   returns either a new SymbolTable and List of GameState Options (Right)
    *   or a CompilerError (Left).
    * @param nargs Exact number of arguments that the function should take
    *   (its arity). If the function isn't called with this exact number of
    *   arguments, it will return Left[CompilerError.ArityError].
    * @return A new SymbolDescriptor.Func
    */
  case class Func private (id: String, func: (Seq[SExpr], SymbolTable) => Either[CompilerError, (SymbolTable, Option[List[GameState]])]) extends SymbolDescriptor
  object Func:
    def apply(
      name: String,
      func: (Seq[SExpr], SymbolTable) => Either[CompilerError, (SymbolTable, Option[List[GameState]])],
      nargs: Int
    ): Func =
      val arityCheckWrapper = (sexpr: Seq[SExpr], symbols: SymbolTable) =>
        if sexpr.length == nargs
        then func(sexpr, symbols)
        else Left(CompilerError.ArityError(name, Seq(nargs), sexpr.length))

      Func(name, arityCheckWrapper)

  /**
    * @param name The name/id of the Func.
    * @param func A Scala function that implements the corresponding GRL function.
    *   The function accepts a Seq of SExprs from the AST and a SymbolTable and
    *   returns either a new SymbolTable and List of GameState Options (Right)
    *   or a CompilerError (Left).
    * @param minArgs The minimum number of arguments that this function may accept.
    *   If the function is called with fewer arguments, it will return
    *   Left[CompilerError.ArityError].
    * @param maxArgs The maximum number of argumnets that this function may accept.
    *   If this is not None, and the function is called with a greater number of
    *   arguments, it will return Left[CompilerError.ArityError].
    * @return A new SymbolDescriptor.Func
    */
    def apply(
      name: String,
      func: (Seq[SExpr], SymbolTable) => Either[CompilerError, (SymbolTable, Option[List[GameState]])],
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

  /**
    * Represents a hand of cards in Gin Rummy.
    *
    * @param cards The Seq of Cards to construct the hand from. Must contain
    *   exactly 10 or 11 cards.
    * @return Either a new SymbolDescriptor.Hand (Right) or a CompilerError(Left).
    */
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

    /** Convert this Hand to a List of Cards. */
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
