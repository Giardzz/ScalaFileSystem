package com.scala.commands
import com.scala.files.DirEntry
import com.scala.filesystem.State
import jdk.javadoc.internal.doclets.formats.html.Contents

class Ls extends Command {
  override def apply(state: State): State = {
    val contents = state.wd.contents
    val niceOutput = createNiceOutput(contents)
    state.setMessage(niceOutput)
  }

  def createNiceOutput(contents: List[DirEntry]): String = {
    if(contents.isEmpty) ""
    else {
      val entry = contents.head
      entry.name + "[" + entry.getType + "]\n"  + createNiceOutput(contents.tail)
    }
  }

}
