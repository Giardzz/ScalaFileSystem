package com.scala.commands
import com.scala.files.{DirEntry, Directory}
import com.scala.filesystem.State

abstract class CreateEntry(name: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage("Entry " + name + " already exists")
    } else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(name + " must not contain separators")
    } else if (checkIllegal(name)) {
      state.setMessage(name + ": illegal entry name!")
    } else {
      doCreateEntry(state, name)
    }

  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doCreateEntry(state: State, name: String): State = {
    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }


    val wd = state.wd

    //1 get all the directories in fullpath
    val allDirsInPath = wd.getAllFoldersInPath

    //2 create new entry in the working directory
    //val newDirectory = Directory.empty(wd.path, name)
    val newEntry: DirEntry = createSpecificEntry(state)
    //3 update the whole dir structure from the root (dir structure is immutable)
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)

    //4 find new working directory instance given wd's full path, in the new directory structure
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)

  }

  def createSpecificEntry(state: State): DirEntry
}
