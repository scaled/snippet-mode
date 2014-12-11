//
// Scaled Snippet Mode - a Scaled minor mode for inserting code snippets
// http://github.com/scaled/snippet-mode/blob/master/LICENSE

package scaled.snippet

import scaled._
import scaled.code.{CodeConfig, Commenter}
import scaled.grammar.{Grammar, GrammarCodeMode, GrammarConfig}
import scaled.util.Resource

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
       desc="""
A major mode for editing Snippet (.snip) files.

A .snip file consists of an optional %include directive, followed by one or more snippet
definitions.

Snippet files are loaded based on the name of the major mode in which the snippets will be used,
so the %include directive allows a major mode's snippet file to include additional shared snippets.
For example:

%include: c-like c-comments

The above would cause `c-like.snip` and `c-comments.snip` to be loaded in addition to the snippets
in the current file.

Here is an example snippet definition:
```
%name: for loop
%keys: for
%syns: default
%line: blank
for (int ${1:ii} = ${2:0}, ${3:ll} = ${4:MAX}; $1 < $3; $1++) {
$0
}
```

Definitions must always start with a %name directive, which provides a human-friendly name. Next
must follow a %keys directive which specifies a space separated list of strings which will trigger
this snippet.

%syns is an optional filter which restricts the snippet to activating only when the syntax at the
point matches that of the snippet. This can be used to restrict certain snippets to operate only
inside comments, for example.

Syntaxes must be a space separated list of one or more of the following:
```
default doc_comment line_comment string_lit char_lit other_lit
```
The default when no %syns directive is present is to match any syntax.

%line restricts snippets from activating unless the line at the point meets certain criteria:
```
`blank`  - indicates that the line must contain nothing except whitespace
`eol`    - only activates a snippet if the point is at the end of the line
`inline` - indicates that the snippet can be expanded anywhere in line
```
`blank` is the default and is used if no %line directive is present.

After the directives comes the snippet template. This is the exact text inserted for the snippet
with "holes" which will be filled in by the user at snippet instantiation time. There are four
kinds of holes:

```
$N - a simple hole with no default value
${N:default} - a hole which supplies a default value
$0 - the exit point of the snippet; the point will be placed here when the snippet is completed
```

The final kind of hole is a mirror. If $N is repeated in the template text, all occurrences after
the first will automatically be populated with the same value supplied by the user for the first
occurrence. If one of the occurrences has a default value, that occurrence will become the main
hole (regardless of whether it is first in the template) and all other occurrences will mirror that
hole.

If no $0 is supplied, the exit point of a snippet is the end of the template text. If $0 is at
the end of the last line of the snippet, no line break will be inserted after the final line of
the snippet. This allows one to define single line "inline" snippets easily. For example:

```
%name: Ternary if
%keys: ?:
${1:cond} ? ${2:if} : ${3:else}$0
```

TODO: describe mirror transforms, once those are implemented.

Snippet definitions must be separated by a single blank line. Any blank lines beyond one will be
included as part of the previous snippet template.
""")
class SnipMode (env :Env) extends GrammarCodeMode(env) {

  override def configDefs = SnipConfig :: super.configDefs
  override def grammars = SnipConfig.grammars.get
  override def effacers = SnipConfig.effacers
  override def syntaxers = SnipConfig.syntaxers

  override val commenter = new Commenter() {
    override def linePrefix = "#"
  }

  @Fn("Inserts the stock snippets for the mode whose snippets are being edited.")
  def insertStockSnippets () {
    val name = buffer.store.name
    getClass.getClassLoader.getResource(s"snippets/$name") match {
      case null => abort(s"No stock snippets named '$name'.")
      case url  => Resource.readLines(url.openStream) foreach { ll =>
          buffer.insertLine(buffer.end, Line(ll)) }
    }
  }
}
