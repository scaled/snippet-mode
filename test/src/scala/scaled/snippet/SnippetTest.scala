//
// Scaled Snippet Mode - a Scaled minor mode for inserting code snippets
// http://github.com/scaled/snippet-mode/blob/master/LICENSE

package scaled.snippet

import org.junit.Assert._
import org.junit._
import scaled._

class SnippetTest {

  val classSnip = List(
    "%name: class Foo {}",
    "%keys: cls class",
    "class ${1:Class} {",
    "$0",
    "}"
  )

  val javaSnips = List(
    "%modes: java",
    ""
  ) ++ classSnip

  @Test def testParse () :Unit = {
    Snippet.parseSnippet(classSnip, println(_))
  }
}
