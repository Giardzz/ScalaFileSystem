package com.scala.commands
import com.scala.files.Directory
import com.scala.filesystem.State

import scala.reflect.io.Path

class Rm(name: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.wd

    val absolutePath = {
      if(name.startsWith(Directory.SEPARATOR)) name
      else if(wd.isRoot) wd.path + name
      else wd.path + Directory.SEPARATOR + name
    }

    if(Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("Not supported")
    else
      doRm(state, absolutePath)

  }

  def doRm(state: State, path: String): State = {
    def rmHelper(currentDirectory: Directory, path: List[String]): Directory = {
      if(path.isEmpty) currentDirectory
      else if(path.tail.isEmpty) currentDirectory.removeEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if(!nextDir.isDirectory) currentDirectory
        else {
          val newNextDir = rmHelper(nextDir.asDirectory, path.tail)
          if(newNextDir == nextDir) currentDirectory
          else currentDirectory.replaceEntry(path.head, newNextDir)
        }
      }
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)
    if(newRoot == state.root)
      state.setMessage(path + ": no such file or directory")
    else
      State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))
  }
}
