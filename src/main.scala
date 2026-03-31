/*
 * Main entry point for the Gin Rummy Language (GRL) compiler.
 *
 * Copyright 2026 Jerrad Michael Genson
 * License: https://github.com/jerradmgenson/gin-rummy-solver/blob/main/LICENSE
 */

import fastparse._

@main
def main(code: String) =
  parse(code, p => program(using p)) match
    case Parsed.Failure(_, _, extra) => println(extra.trace().longMsg)
    case Parsed.Success(program, _)  => evaluateProgram(program) match
      case Right(result) =>
        result.foreach(println(_))
        println("SUCCESS")
      case Left(compilerError) => println(s"FAILURE: ${compilerError.description}")
