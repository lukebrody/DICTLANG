import scala.io.StdIn.readLine

@main def hello: Unit = {
  while (true) {
      val input = readLine()
      val tokens = tokenize.tokenize(input)
      println(tokens)
      tokens.foreach { tokens =>
        val ast = parse.Value.parse(parse.Success(null, tokens))

        ast match {
          case success: parse.Success[_] if success.rest.isEmpty => println(success)
          case success: parse.Success[_] => println(parse.Failure("Leftovers", success.rest))
          case failure: parse.Failure => println(failure)
        }
      }
  }
}
