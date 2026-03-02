import fastparse._, MultiLineWhitespace._

sealed trait SExpr
object SExpr:
  case class Ident(name: String) extends SExpr
  case class Number(value: Int) extends SExpr
  case class SList(sexpr: Seq[SExpr]) extends SExpr

def program[$: P] = P(Start ~ sexpr.rep ~ End)
def sexpr[$: P] = P(atom | slist)
def slist[$: P]: P[SExpr.SList] = P("(" ~/ sexpr.rep ~/ ")").map(s => SExpr.SList(s))
def atom[$: P] = P(ident | number)
def ident[$: P] = P(CharIn("0-9", "a-zA-Z", "_$*\\-")
  .repX(1)
  .!
  .filter(_.toIntOption.isEmpty)
  .map(s => SExpr.Ident(s)))
def number[$: P]: P[SExpr.Number] = P("-".? ~~ CharIn("0-9").repX(1)).!.map(s => SExpr.Number(s.toInt))

@main
def main(code: String) =
  val Parsed.Success(result, index) = parse(code, p => program(using p))
  println(result)
