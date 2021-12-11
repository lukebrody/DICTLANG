import scala.io.StdIn.readLine

@main def hello: Unit = {
  while (true) {
      val input = readLine()
      val tokens = tokenize.tokenize(input)
      println(tokens)
      tokens.foreach { tokens =>
        val ast = parse.Value.parse(parse.Success(null, tokens, Set.empty))
        println(ast)
      }
  }
}
