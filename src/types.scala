/*
 * Defines GRL compile-time errors and types for the GRL language itself.
 *
 * Copyright 2026 Jerrad Michael Genson
 * License: https://github.com/jerradmgenson/gin-rummy-solver/blob/main/LICENSE
 */

/**
  * Corresponds to the types of values intrinsic that exist in GRL.
  */
enum GRLType:
  case Card, Wildcard, Template, Integer, GameState, Function

/**
  * Types of errors that can be returned by the compiler.
  *
  * Some of these are self-descriptive, so only the non-obvious ones are
  * documented. Except for InternalError, all of these errors indicate a
  * problem with the GRL problem that results in it failing to compile.
  */
enum CompilerError:
  case SyntaxError(desc: String)
  case ArityError(funcName: String, expected: Seq[Int], found: Int)
  case TypeError(expected: Seq[GRLType], found: Option[GRLType])
  /** Indicates that the type of a value is valid, but the particular value is invalid. */
  case ValueError(desc: String)
  /** Indicates a reference to an identifier that is not defined. */
  case UndefinedError(symbolName: String)
  /** Indicates that the user is attempting to redefine a built-in identifier. */
  case RedefinitionError(symbolName: String)
  /**
    * Indicates an error with the compiler itself rather than the GRL program.
    * This error should never occur. If it does occur, there is a bug with the
    * compiler.
    */
  case InternalError(desc: String)

  /** Return a sub-type specific description for each CompilerError case. */
  def description: String = this match
    case SyntaxError(desc) => desc
    case ArityError(funcName, expected, found) =>
      val expectedStr = expected.mkString("[", ", ", "]")
      s"Incorrect number of arguments to $funcName. Expected: $expectedStr Found: $found"
    case TypeError(expected, found) =>
      val expectedStr = expected.mkString("[", ", ", "]")
      found match
        case Some(f) => s"Incorrect type. Expected: $expectedStr Found: $f"
        case None    => s"Incorrect type. Expected: $expectedStr"
    case ValueError(desc) => desc
    case UndefinedError(symbolName) => s"`$symbolName` is not defined."
    case RedefinitionError(symbolName) => s"Can not redefine built-in name `$symbolName`."
    case InternalError(desc) => desc
