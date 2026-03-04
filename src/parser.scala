import fastparse._, ScriptWhitespace._
import scala.annotation.{alpha, tailrec}

sealed trait SExpr
object SExpr:
  case class Ident(name: String) extends SExpr
  case class Wildcard(invariant: Option[Char]) extends SExpr
  case class Number(value: Int) extends SExpr
  case class SList(sexpr: Seq[SExpr]) extends SExpr

def program[$: P] = P(Start ~ sexpr.rep ~ End)
def sexpr[$: P] = P(atom | slist)
def slist[$: P]: P[SExpr.SList] = P("(" ~/ sexpr.rep ~/ ")").map(s => SExpr.SList(s))
def atom[$: P] = P((wildcard | ident | number) ~~/ &(whitespace | ")" | End))
def ident[$: P] = P((CharIn("_$\\-") | alphanumeric)
  .repX(1)
  .!
  .filter(_.toIntOption.isEmpty)
  .map(s => SExpr.Ident(s)))
def number[$: P] = P("-".? ~~ CharIn("0-9").repX(1)).!.map(s => SExpr.Number(s.toInt))
def wildcard[$: P] = P(("*" ~~/ alphanumeric.?) | (alphanumeric ~~ "*")).!.map(_ match {
  case "*" => SExpr.Wildcard(None)
  case s"*$value" => SExpr.Wildcard(Some(value.head))
  case s"$value*" => SExpr.Wildcard(Some(value.head))
})
def alphanumeric[$: P] = P(CharIn("0-9a-zA-Z"))
def whitespace[$: P] = P(CharIn(" \t\r\n"))

@main
def main(code: String) =
  parse(code, p => program(using p)) match
    case Parsed.Failure(_, _, extra) => println(extra.trace().longMsg)
    case Parsed.Success(program, _) => evaluateProgram(program).foreach(println(_))

def evaluateProgram(program: Seq[SExpr], symbols: SymbolTable = SymbolTable()): List[String] = program.headOption match
  case None => List[String]()
  case Some(SExpr.SList(sexpr)) =>
    val (commands, symbols) = evaluateSList(SExpr.SList(sexpr), symbols)
    evaluateProgram(program.tail, symbols)

class SymbolTable(stack: List[Map[String, SymbolDescriptor]] = List(Map[String, SymbolDescriptor])):
  def addFrame() = SymbolTable(Map[String, SymbolDescriptor]() :: stack)
  def removeFrame() = SymbolTable(stack.tail)
  def get(name: String) =
    @tailrec
    def aux(stack: List[Map[String, SymbolDescriptor]]): Option[SymbolDescriptor] = stack.headOption match
      case None => None
      case Some(stackFrame) => stackFrame.get(name) match
        case None => aux(stack.tail)
        case Some(symbol) => Some(symbol)
    aux(stack)

  def set(name: String, symbol: SymbolDescriptor) = stack.headOption match
    case None => None
    case Some(stackFrame) => Some((stackFrame + (name -> symbol)) :: stack.tail)


sealed trait SymbolDescriptor { def id: String }
object SymbolDescriptor:
  case class CardList(id: String, cards: List[Card], wildcards: List[SExpr.Wildcard], templates: List[String]) extends SymbolDescriptor
  case class Template(id: String, cards: List[Card]) extends SymbolDescriptor
  case class Config(id: String, value: ConfigOption) extends SymbolDescriptor
  case class Game(id: String)


sealed trait ConfigOption
object ConfigOption:
  case class Score(myScore: Int, theirScore: Int) extends ConfigOption
  case class EndScore(value: Int) extends ConfigOption
  case class Gin(value: Int) extends ConfigOption
  case class BigGin(value: Int) extends ConfigOption
  case class Undercut(value: Int) extends ConfigOption
  case class KnockThreshold(value: Int) extends ConfigOption
  case class RemainingStock(value: Int) extends ConfigOption


case class Card(rank: Rank, suit: Suit)
enum Suit:
  case Spades, Hearts, Diamonds, Clubs

enum Rank:
  case Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King
