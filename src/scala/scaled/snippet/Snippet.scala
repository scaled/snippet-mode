//
// Scaled Snippet Mode - a Scaled minor mode for inserting code snippets
// http://github.com/scaled/snippet-mode/blob/master/LICENSE

package scaled.snippet

import java.lang.{StringBuffer => JStringBuilder}
import java.util.regex.{Pattern, Matcher}
import scaled._
import scaled.util.Resource

/** Tags a primary hole in a snippet. */
case class Hole (loc :Loc, deflen :Int, mirrors :Seq[Loc])
// TODO: reify mirrors, and include transforms

/** Metadata for a snippet. */
case class Snippet (
  /** A user friendly name for this snippet. */
  name :String,
  /** The trigger strings for this snippet. */
  triggers :Set[String],
  /** The syntax predicate for this snippet. */
  syntaxes :Syntax => Boolean,
  /** The raw template for this snippet. */
  template :Seq[String]) {

  /** Inserts this snippet into `buffer` at `loc`. Returns the list of holes in the inserted
    * snippet. The final hole will be the "exit" point of the snippet (indicated by `$0` in the raw
    * template).
    * @param ifn a fn to compute the indent prefix for a row.
    */
  def insert (buffer :Buffer, loc :Loc, ifn :Int => String) :(Seq[Hole], Loc) = {
    // we delay parsing until insertion because it's not that bad to parse one snippet just before
    // we insert it, but parsing hundreds of snippets at startup would be troublesome
    val holes = Seq.builder[(Int,Int,Loc)]()
    var end = loc ; var exit = Loc.None
    val iter = template.iterator() ; while (iter.hasNext) {
      val indent = if (end > loc) ifn(end.row) else "" // no indent on first line
      val text = indent + iter.next
      val m = Snippet.HolePat.matcher(text) ; var pos = 0
      var lb :Line.Builder = null
      while (m.find(pos)) {
        val mpos = m.start ; val pre = text.substring(pos, mpos)
        if (lb == null) lb = Line.builder(pre)
        else lb += pre
        val g2 = m.group(2) ; val g3 = m.group(3)
        val id = (if (g2 == null) g3 else g2).toInt
        // if the id is zero, then this is the exit point of the snippet
        if (id == 0) exit = end + (0, lb.length)
        else {
          // if we have group(4) then it's a hole with default, otherwise its just a hole
          val defstr = m.group(4)
          def add (str :String, len :Int) {
            holes += (id, len, end + (0, lb.length))
            lb += str
          }
          if (defstr == null) add("", 0) else add(defstr, defstr.length)
        }
        pos = m.end
      }
      val line = if (lb == null) Line(text) else lb.append(text.substring(pos)).build()
      end = buffer.insertLine(end, line)
    }

    // if there was no $0, put one at the end of the snippet
    if (exit == Loc.None) exit = end

    // now group the holes by id and figure out which are mirrors
    val hmap = holes.groupBy(_._1)
    val sb = Seq.builder[Hole](hmap.size+1)
    hmap foreach { (_, holes) =>
      // determine which is the primary hole:
      // either the one with the default value, or the first one in the list
      val main = holes.find(_._2 > 0) getOrElse holes.head
      sb += Hole(main._3, main._2, holes.filterNot(_ == main).map(_._3))
    }
    sb += Hole(exit, 0, Seq()) // add the faux "exit" hole
    (sb.build(), end)
  }

  override def toString = s"Snippet($name, $triggers)\n${template.mkString("\n")}"
}

object Snippet {

  /** Contains the data from a `.snip` file. */
  case class Source (modes :Set[String], snippets :Seq[Snippet])

  private val ModesKey = "%modes:"
  private val NameKey  = "%name:"
  private val KeysKey  = "%keys:"
  private val SynsKey  = "%syns:"
  private val AllSyntaxes = (_ :Syntax) => true

  /** A fn which parses a set of .snip files packaged into a resource. */
  val parseRsrc = (rsrc :Resource) => rsrc.lines.map(parseSource)

  /** Parses `lines`, which ostensibly represent the contents of a `.snip` file. That looks
    * something like:
    * ```
    * %modes: java scala
    * %name: if else block
    * %keys: ife ifel
    * %syns: default
    * if (${1:condition}) $2 else $3
    * (optional blank line)
    * %name: ...
    * ```
    */
  def parseSource (lines :Iterable[String]) :Source = {
    val snips = Seq.builder[Snippet]()
    val modes = Set.builder[String]()
    var ll = lines.toList ; while (!ll.isEmpty) {
      val tline = ll.head
      if (tline startsWith NameKey) ll = parseSnippet(ll, snips += _)
      else {
        if (tline startsWith ModesKey) modes ++= getval(tline, ModesKey).split(" ")
        else if ({ val tt = tline.trim ; (tt startsWith "#") || (tt.length == 0)}) () // skip
        ll = ll.tail
      }
    }
    Source(modes.build(), snips.build())
  }

  private def getval (line :String, key :String) = line.substring(key.length).trim

  private val HolePat = Pattern.compile("""\$(([0-9]+)|\{([0-9]+):([^}]+)\})""")

  private[snippet] def parseSnippet (lines :List[String], recv :Snippet => Unit) = {
    var ll = lines // we'll move the line cursor along as we parse

    var name = ""
    var triggers = Set[String]()
    var syntaxes = AllSyntaxes
    val bbuf = Seq.builder[String]()

    def parseSyntaxes (syns :String) = AllSyntaxes // TODO

    // first parse the parameters
    var cont = true ; while (cont && !ll.isEmpty) {
      val tline = ll.head
      if (tline startsWith NameKey) name = getval(tline, NameKey)
      else if (tline startsWith KeysKey) triggers = Set.from(getval(tline, KeysKey).split(" "))
      else if (tline startsWith SynsKey) syntaxes = parseSyntaxes(getval(tline, SynsKey))
      // else complain if line looks like %foo:?
      else cont = false
      if (cont) ll = ll.tail
    }

    // next parse the body
    cont = true ; while (cont && !ll.isEmpty) {
      val tline = ll.head
      if (tline startsWith NameKey) cont = false
      else bbuf += ll.head
      if (cont) ll = ll.tail
    }
    // if the last line of the snippet is blank, remove it
    if (bbuf.last.length == 0) bbuf.removeAt(bbuf.length-1)

    // TODO: validate things?

    // finally create a snippet and pass it to the receiver fn
    recv(Snippet(name, triggers, syntaxes, bbuf.build()))

    ll
  }

}
