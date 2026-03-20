import fastparse._

@main
def main(code: String) =
  parse(code, p => program(using p)) match
    case Parsed.Failure(_, _, extra) => println(extra.trace().longMsg)
    case Parsed.Success(program, _)  => evaluateProgram(program) match
      case Right(result)       => result.foreach(println(_))
      case Left(compilerError) => println(s"FAILURE: ${compilerError.description}")
