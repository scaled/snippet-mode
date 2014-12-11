//
// Scaled Snippet Mode - a Scaled minor mode for inserting code snippets
// http://github.com/scaled/snippet-mode/blob/master/LICENSE

package scaled.snippet

import scaled._

/** Provides access to the database of snippets to snippet modes. */
@Service(name="snippet", impl="SnippetManager", desc="Manages snippet database.")
trait SnippetService {

  /** Returns all snippets that are applicable to `mode`, from highest to lowest precedence.
    * @param scope the config scope for the buffer using snippets.
    */
  def resolveSnippets (scope :Config.Scope, mode :String) :Seq[Snippet]
}
