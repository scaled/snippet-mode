//
// Scaled Snippet Mode - a Scaled minor mode for inserting code snippets
// http://github.com/scaled/snippet-mode/blob/master/LICENSE

package scaled.snippet

import com.google.common.base.Charsets
import com.google.common.io.CharStreams
import java.io.InputStreamReader
import java.net.URL
import java.nio.file.{Path, Files}
import java.util.HashSet
import scaled._

/** Implements [[SnippetService]]. Keeps track of stuff. */
class SnippetManager (msvc :MetaService, editor :Editor)
    extends AbstractService with SnippetService {

  private val userSnipCache = Mutable.cacheMap[(String,Path),Seq[Snippet]]((readDirSnips _).tupled)
  private val stockSnipCache = Mutable.cacheMap[String,Seq[Snippet]](readRsrcSnips)

  override def didStartup () {}
  override def willShutdown () {}

  override def resolveSnippets (scope :Config.Scope, mode :String) = {
    val snips = Seq.builder[Snippet]()
    // first look through all the config directories, adding any snippets from there
    scope.toList.map(_.root.resolve("snippets")) foreach { dir =>
      snips ++= userSnipCache.get((mode, dir))
    }

    // TODO: add snips from any registered "snippet database" directories

    // last add any global configs
    snips ++= stockSnipCache.get(mode)
    snips.build()
  }

  private def readDirSnips (mode :String, dir :Path) = readSnips(mode) { name =>
    val path = dir.resolve(s"$name.snip")
    if (Files.exists(path)) Some(Files.readAllLines(path)) else None
  }

  private def readRsrcSnips (mode :String) = readSnips(mode) { name =>
    val url = getClass.getClassLoader.getResource(s"snippets/$name.snip")
    if (url == null) None else Some(readLines(url))
  }

  private def readSnips (mode :String)(reader :String => Option[Iterable[String]]) = {
    val snips = Seq.builder[Snippet]()
    val seen = new HashSet[String]()
    def add (name :String) :Unit = if (seen.add(name)) reader(name) foreach { lines =>
      val incls = Snippet.parse(lines, snips)
      incls foreach add
    }
    add(mode)
    snips.build()
  }

  private def readLines (url :URL) = {
    val in = url.openStream()
    try CharStreams.readLines(new InputStreamReader(in, Charsets.UTF_8))
    finally in.close()
  }
}
