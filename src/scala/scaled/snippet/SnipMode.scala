//
// Scaled Snippet Mode - a Scaled minor mode for inserting code snippets
// http://github.com/scaled/snippet-mode/blob/master/LICENSE

package scaled.snippet

import scaled._
import scaled.code.{CodeConfig, Commenter}
import scaled.grammar.{Grammar, GrammarCodeMode, GrammarConfig}

object SnipConfig extends Config.Defs {
  import CodeConfig._
  import GrammarConfig._

  // map TextMate grammar scopes to Scaled style definitions
  val effacers = List(
    effacer("comment.line", commentStyle),
    effacer("punctuation.line-cont", typeStyle),
    effacer("snippet.name", typeStyle),
    effacer("snippet.trigger", constantStyle),
    effacer("snippet.default", stringStyle),
    effacer("snippet.hole", functionStyle),
    effacer("keyword", keywordStyle)
  )

  // map TextMate grammar scopes to Scaled syntax definitions
  val syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment)
  )

  val grammars = resource("Snip.ndf")(Grammar.parseNDFs)
}

@Major(name="snip",
       tags=Array("code", "project", "snip"),
       pats=Array(".*\\.snip"),
       desc="A major mode for editing Snippet (.snip) files.")
class SnipMode (env :Env) extends GrammarCodeMode(env) {

  override def configDefs = SnipConfig :: super.configDefs
  override def grammars = SnipConfig.grammars.get
  override def effacers = SnipConfig.effacers
  override def syntaxers = SnipConfig.syntaxers

  override val commenter = new Commenter() {
    override def linePrefix = "#"
  }
}
