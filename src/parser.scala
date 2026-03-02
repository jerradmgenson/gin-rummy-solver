import fastparse._, ScriptWhitespace._
import scala.annotation.alpha

sealed trait SExpr
object SExpr:
  case class Ident(name: String) extends SExpr
  case class Wildcard(value: Option[Char], valueFirst: Boolean) extends SExpr
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
  case "*" => SExpr.Wildcard(None, false)
  case s"*$value" => SExpr.Wildcard(Some(value.head), false)
  case s"$value*" => SExpr.Wildcard(Some(value.head), true)
})
def alphanumeric[$: P] = P(CharIn("0-9a-zA-Z"))
def whitespace[$: P] = P(CharIn(" \t\r\n"))

@main
def main(code: String) =
  parse(code, p => program(using p)) match
    case Parsed.Success(value, _) => println(value)
    case Parsed.Failure(_, _, extra) => println(extra.trace().longMsg)
