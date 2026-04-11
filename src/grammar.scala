/*
 * Defines the grammar and implements a parser for Gin Rummy Language (GRL).
 *
 * Copyright 2026 Jerrad Michael Genson
 * License: https://github.com/jerradmgenson/gin-rummy-solver/blob/main/LICENSE
 */

import fastparse._, ScriptWhitespace._

/**
  * Represents nodes of the GRL AST.
  */
enum SExpr:
  case Ident(val name: String) extends SExpr
  case Wildcard(val invariant: Option[Char]) extends SExpr
  case Number(val value: Int) extends SExpr
  case SList(val sexpr: Seq[SExpr]) extends SExpr

/** Non-terminal: represents a complete GRL program. This is the parser's entrypoint. */
def program[$: P] = P(Start ~ sexpr.rep ~ End)

/** Non-terminal: represents an s-expression. */
def sexpr[$: P] = P(atom | slist)

/** Non-terminal: represents a list of s-expressions. */
def slist[$: P]: P[SExpr.SList] =
  P("(" ~/ sexpr.rep ~/ ")").map(SExpr.SList(_))

/** Non-terminal: represents an atom. */
def atom[$: P] = P((wildcard | ident | number) ~~/ &(whitespace | "(" | ")" | End))

/** Terminal: represents an identifier. */
def ident[$: P] = P(
  (CharIn("_$\\-") | alphanumeric)
    .repX(1)
    .!
    .filter(_.toIntOption.isEmpty)
    .map(SExpr.Ident(_))
)

/** Terminal: represents an integer value. */
def number[$: P] =
  P("-".? ~~ CharIn("0-9").repX(1)).!.map(s => SExpr.Number(s.toInt))

/** Terminal: represents a wildcard value. */
def wildcard[$: P] =
  P(("*" ~~/ alphanumeric.?) | (alphanumeric ~~ "*")).!.map {
    case "*"        => SExpr.Wildcard(None)
    case s"*$value" => SExpr.Wildcard(Some(value.head))
    case s"$value*" => SExpr.Wildcard(Some(value.head))
  }

/** Token: alphanumeric characters. */
def alphanumeric[$: P] = P(CharIn("0-9a-zA-Z"))

/** Token: whitespace characters. */
def whitespace[$: P] = P(CharIn(" \t\r\n"))
