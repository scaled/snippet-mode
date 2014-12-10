//
// Scaled Snippet Mode - a Scaled minor mode for inserting code snippets
// http://github.com/scaled/snippet-mode/blob/master/LICENSE

package scaled.snippet

import scaled._
import scaled.code.CodeMode
import scaled.util.AnchorSet

object SnippetConfig extends Config.Defs {

  /** The CSS style applied to all holes other than the active hole. */
  val holeStyle = "holeFace"

  /** The CSS style applied to the hole being currently edited. */
  val activeHoleStyle = "activeHoleFace"

  /** The CSS style applied to all holes which mirror the active hole. */
  val mirrorStyle = "mirrorFace"

  /** TEMP: Our built-in snippets converted into [[Snippet.Source]] objects. */
  val sources = resource(Seq("snippets/java.snip"))(Snippet.parseRsrc)
}

@Minor(name="snippet", tags=Array("text", "code"),
       desc="""Provides support for insertion and population of text/code snippets.""")
class SnippetMode (env :Env, major :MajorMode) extends MinorMode(env) {
  import SnippetConfig._

  override def configDefs = SnippetConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/snippet.css") :: super.stylesheets

  override def keymap = super.keymap.
    bind("expand-or-next-snippet", "TAB").
    bind("prev-snippet", "S-TAB");

  // TODO: marge snippets from all sources, more complex stuff, etc. etc.
  val snipsTrigs = {
    val bb = Seq.builder[(Matcher,Int,Snippet)]()
    for (src <- sources.get ; if (src.modes(major.name)) ; snip <- src.snippets ;
         trig <- snip.triggers) bb += (Matcher.exact(trig), trig.length, snip)
    bb.sortBy(-_._2) // sort by longest to shortest trigger
  }

  /** A snippet and the loc at which it was activated. */
  class ActiveSnip (holes :Seq[Hole], initStart :Loc, initEnd :Loc) {
    val anchors = new AnchorSet(buffer)

    // track the start and end of the entire snippet
    val startA = anchors.add(initStart, true)
    val endA = anchors.add(initEnd, false)

    // track the bounds of each hole and mirror in the snippet
    class ActiveHole (hole :Hole) extends Region {
      val startA = anchors.add(hole.loc, true)
      def start = startA.loc
      val endA = anchors.add(hole.loc + (0, hole.deflen), false)
      def end = endA.loc

      var isDefault = (hole.deflen > 0)

      def activate () {
        buffer.addTag(activeHoleStyle, this)
        view.point() = if (isDefault) start else end
      }
      def deactivate () {
        buffer.removeTag(activeHoleStyle, this)
      }
      // TODO: mirrors

      override def toString = s"H:[$start, $end):$isDefault"
    }
    val aholes = holes map { new ActiveHole(_) }

    val activeHole = OptValue[ActiveHole]()
    activeHole onChange { (nh, oh) =>
      oh foreach { _.deactivate() }
      nh foreach { _.activate() }
    }

    def activate () {
      // println(s"activating $this")
      // TODO: reindent(start)?
      activeHole() = aholes.head
      _conn = buffer.edited onValue onEdit
    }

    def deactivate () {
      // println(s"deactivating $this")
      _conn.close()
      activeHole.clear()
    }

    def nextHole () {
      val hidx = aholes.indexOf(activeHole())
      if (hidx < aholes.size-2) activeHole() = aholes(hidx+1)
      // if we're on the penultimate hole, then the next hole is the exit and deactivates us
      else {
        activeSnip.clear()
        view.point() = aholes(hidx+1).start
        ifCode(_.reindentAtPoint()) // TODO: is this always a good idea?
      }
    }

    def prevHole () {
      val hidx = aholes.indexOf(activeHole())
      if (hidx > 0) activeHole() = aholes(hidx-1)
    }

    private var _conn :Connection = _
    private def onEdit (edit :Buffer.Edit) = edit match {
      case Buffer.Insert(estart, eend) =>
        val ah = activeHole()
        // println(s"onInsert($estart, $eend) <- $ah")
        // if they edit outside the bounds of the active hole, deactivate the snippet; they
        // better be done because we can't be keeping track of all that craziness
        if (!ah.contains(estart)) activeSnip.clear()
        else {
          // retag the (now larger) active hole with its style; if we just tag the newly inserted
          // segment, we get pesky outlines because tag spans are not merged automatically
          buffer.removeTag(activeHoleStyle, ah)
          buffer.addTag(activeHoleStyle, ah)

          // if this hole currently contains its default value, queue it up to be cleared (we
          // can't clear it immediately because we're processing a buffer edit right now)
          if (ah.isDefault) {
            // replace the whole hole region with this insertion; that will remove the default and
            // replace it with just what was inserted
            ah.isDefault = false
            // do our replacement in the didInvoke hook so that its combined with the current edit
            // for undo purposes; we can't do it now because we're dispatching an edit
            (disp.didInvoke onEmit { buffer.replace(ah, buffer.region(estart, eend)) }).once()
          }
          // TODO: update active hole mirrors
        }

      case Buffer.Delete(estart, eend, deleted) =>
        val ah = activeHole()
        // println(s"onDelete($estart, $eend) (${deleted.map(_.tags)}) <- $ah")
        // if they deleted any text that is not marked with active hole face, then they deleted
        // something outside the active hole, so we assume they're done with this snippet
        def notActive (line :LineV) = !line.tagsAt(0).exists(
          t => t.tag == activeHoleStyle && t.start == 0 && t.end == line.length)
        if (deleted.exists(notActive)) activeSnip.clear()
        // TODO: else { update active hole mirrors }

      case _ => // don't care about transforms
    }

    override def toString = s"Snip [${startA.loc}:${endA.loc}) $aholes"
  }

  /** The currently active snippet, if any. */
  val activeSnip = OptValue[ActiveSnip]()
  def reqActiveSnip = activeSnip getOrElse abort("No active snippet.")
  activeSnip onChange { (ns, os) =>
    os foreach { _.deactivate() }
    ns foreach { _.activate() }
  }

  @Fn("""If there is no active snippet, attemps to expand the snippet at the point.
         If there is an active snippet, advances to the next hole in the snippet.""")
  def expandOrNextSnippet () :Boolean = activeSnip.getOption match {
    case None       => expandSnippet()
    case Some(snip) => snip.nextHole() ; true
  }

  @Fn("Expands the snippet at the point, if any.")
  def expandSnippet () :Boolean = {
    // TODO: make this more efficient (build trie from reversed trigger strings, match backward
    // char by char)
    val p = view.point() ; val col = p.col ; val line = buffer.line(p)
    val iter = snipsTrigs.iterator() ; while (iter.hasNext) {
      val (trig, tlen, snip) = iter.next()
      if ((tlen <= col) && line.matches(trig, col-tlen)) {
        val start = p.atCol(p.col-tlen)
        buffer.delete(start, p) // delete the trigger
        val (holes, end) = snip.insert(buffer, start, computeIndent) // insert the snippet
        activeSnip() = new ActiveSnip(holes, start, end) // and start the heavy machinery
        return true
      }
    }
    false
  }

  @Fn("""Moves to the next hole in the active snippet. If the point is currently on the last
         hole, it will be moved to the snippet's "exit" and the snippet will be deactivated.""")
  def nextSnippet () :Unit = reqActiveSnip.nextHole()

  @Fn("""Moves to the previous hole in the active snippet.
         NOOPs if the point is already on the first hole in the snippet.""")
  def prevSnippet () :Unit = reqActiveSnip.prevHole()

  @Fn("""Deactivates the active snippet.""")
  def deactivateSnippet () :Unit = activeSnip.clear()

  private val computeIndent = major match {
    case code :CodeMode => (row :Int) => " " * code.computeIndent(row)
    case _              => (row :Int) => ""
  }

  private def ifCode (fn :CodeMode => Unit) :Unit = major match {
    case code :CodeMode => fn(code)
    case _ => // oh well, nada
  }

  private def add (loc :Loc, snipLoc :Loc) :Loc = snipLoc.row match {
    case 0 => loc.atCol(loc.col + snipLoc.col)
    case n => Loc(loc.row + snipLoc.row, snipLoc.col)
  }
}
