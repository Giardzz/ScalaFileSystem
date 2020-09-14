package com.scala.commands
import com.scala.files.{DirEntry, Directory}
import com.scala.filesystem.State

class Mkdir(name: String) extends CreateEntry(name) {
  override def createSpecificEntry(state: State): DirEntry =
    Directory.empty(state.wd.path, name)
}
