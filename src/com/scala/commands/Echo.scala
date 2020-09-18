package com.scala.commands
import com.scala.files.{Directory, File}
import com.scala.filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {
  override def apply(state: State): State = {

    def createContent(args: Array[String], topIndex: Int): String = {
      @tailrec
      def createContentHelper(currentIndex: Int, accumulator: String): String = {
        if(currentIndex >= topIndex) accumulator
        else createContentHelper(currentIndex + 1, accumulator + " " + args(currentIndex))
      }

      createContentHelper(0, "")
    }

    def getRootAfterEcho(currentDir: Directory, path: List[String], contents: String, append: Boolean): Directory = {
      if(path.isEmpty) currentDir
      else if(path.tail.isEmpty) {
        val dirEntry = currentDir.findEntry(path.head)
        if(dirEntry == null) currentDir.addEntry(new File(currentDir.path, path.head, contents))
        else if(dirEntry.isDirectory) currentDir
        else
          if(append) currentDir.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
          else currentDir.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
      } else {
        val nextDir = currentDir.findEntry(path.head).asDirectory
        val newNextDir = getRootAfterEcho(nextDir, path.tail, contents, append)

        if(newNextDir == nextDir) currentDir
        else currentDir.replaceEntry(path.head, newNextDir)
      }
    }

    def doEcho(state: State, content: String, fileName: String, append: Boolean): State = {
      if(fileName.contains(Directory.SEPARATOR))
        state.setMessage("Echo: file name must not contain a separator")
      else {
        val newRoot: Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ fileName, content, append)
        if(newRoot == state.root)
          state.setMessage("Echo: " + fileName + " no such file")
        else
          State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
      }
    }

    if (args.isEmpty) state
    else if(args.length == 1) state.setMessage(args(0))
    else {
      val operator = args(args.length - 2)
      val fileName = args(args.length - 1)
      val content = createContent(args, args.length - 2)

      if(operator.equals(">>"))
        doEcho(state, content, fileName, true)
      else if(operator.equals(">"))
        doEcho(state, content, fileName, false)
      else
        state.setMessage(createContent(args, args.length ))
    }

  }
}
