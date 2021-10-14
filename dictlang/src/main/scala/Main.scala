import scala.io.StdIn.readLine

@main def hello: Unit = {
  while (true) {
      val input = readLine()
      val tokens = tokenize(input)
      println(tokens)
      tokens.foreach { tokens =>
        val ast = Expression.parse(Success(null, tokens))
        println(ast)
      }
  }
}
