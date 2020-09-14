package com.scala.files

import com.scala.filesystem.FileSystemException

class File(override val parentPath: String, override val name: String, contents: String)
extends DirEntry(parentPath, name){

  override def asDirectory: Directory = throw new FileSystemException("A File cannot be converted to a directory!")
  override def isDirectory: Boolean = false
  override def isFile: Boolean = true
  override def asFile: File = this

  override def getType: String = "File"
}

object File {

  def empty(parentPath: String, name: String): File =
    new File(parentPath, name, "")
}
