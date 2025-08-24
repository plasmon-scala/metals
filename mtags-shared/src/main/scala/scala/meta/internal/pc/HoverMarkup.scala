package scala.meta.internal.pc

object HoverMarkup {

  // VSCode trims the message to ~100000, thus messing the markdown for very long messages.
  // Number based on experiments from 29.09.2023.
  private val MaxHoverBodyLength = 50000

  /**
   * Render the textDocument/hover result into markdown.
   *
   * @param expressionType The type of the expression over the cursor, for example "List[Int]".
   * @param symbolSignature The signature of the symbol over the cursor, for example
   *                        "def map[B](fn: A => B): Option[B]"
   * @param docstring The Scaladoc/Javadoc string for the symbol.
   */
  def apply(
      expressionType: String,
      optSymbolSignature: Option[String],
      docstring: String,
      forceExpressionType: Boolean = false,
      contextInfo: List[String] = Nil,
      markdown: Boolean = true
  ): String = {
    val builder = new StringBuilder()

    var printedScalaStuff = false
    var printedDocstringStuff = false

    def appendCode(title: Option[String], code: String) = {
      title.foreach { title =>
        builder
          .append(if (markdown) "**" else "")
          .append(title)
          .append(if (markdown) "**" else "")
          .append("\n")
      }
      builder
        .append(if (markdown) "```scala\n" else "")
        .append(code)
        .append(if (markdown) "\n```" else "\n")
    }

    if (contextInfo.nonEmpty) {
      appendCode(None, contextInfo.mkString("\n"))
      builder.append("\n\n")
      printedScalaStuff = true
    }

    optSymbolSignature.foreach { symbolSignature =>
      if (symbolSignature.nonEmpty) {
        appendCode(
          if (forceExpressionType) Some("Symbol signature") else None,
          symbolSignature
        )
        printedScalaStuff = true
      }
    }
    if (docstring.nonEmpty) {
      if (printedScalaStuff)
        builder.append("\n\n***\n")
      builder
        .append("\n")
        .append(docstring)
      printedDocstringStuff = true
    }
    if (forceExpressionType || optSymbolSignature.isEmpty) {
      if (printedDocstringStuff || printedScalaStuff)
        builder.append("\n\n***\n")
      appendCode(
        if (optSymbolSignature.isDefined) Some("Expression type") else None,
        expressionType
      )
      builder.append("\n")
      printedScalaStuff = true
    }
    builder.toString()
  }

  private def trimBody(body: String) =
    if (body.length() <= MaxHoverBodyLength) body
    else body.take(MaxHoverBodyLength) + "..."

  def apply(body: String): String = {
    s"""|```scala
        |${trimBody(body)}
        |```""".stripMargin
  }

  def javaHoverMarkup(body: String): String = {
    s"""|```java
        |${trimBody(body)}
        |```""".stripMargin
  }

  def javaHoverMarkup(
      expressionType: String,
      symbolSignature: String,
      docstring: String,
      forceExpressionType: Boolean = false,
      markdown: Boolean = true
  ): String = {
    val builder = new StringBuilder()

    def addCode(title: Option[String], code: String) = {
      title.foreach { title =>
        builder
          .append(if (markdown) "**" else "")
          .append(title)
          .append(if (markdown) "**" else "")
          .append(":\n")
      }
      builder
        .append(if (markdown) "```java\n" else "")
        .append(code)
        .append(if (markdown) "\n```" else "")
    }

    var printedJavaStuff = false
    var printedDocstring = false

    if (symbolSignature.nonEmpty) {
      addCode(
        if (forceExpressionType) Some("Symbol signature") else None,
        symbolSignature
      )
      printedJavaStuff = true
    }

    if (docstring.nonEmpty) {
      if (printedJavaStuff)
        builder.append("\n\n***\n")
      builder
        .append("\n")
        .append(docstring)
      printedDocstring = true
    }
    if (forceExpressionType) {
      if (printedJavaStuff || printedDocstring)
        builder.append("\n\n***\n")
      addCode(Some("Expression type"), expressionType)
      builder.append("\n")
      printedJavaStuff = true
    }

    builder.toString()
  }

}
