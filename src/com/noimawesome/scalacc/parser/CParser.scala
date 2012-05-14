package com.noimawesome.scalacc.parser

import java.io.FileNotFoundException

class CParser(fileLocation: String) {
  var source: String = openFile()

  print ("Can be parsed = " + CheckCSyntax.parse(source))


  def openFile(): String = {
    var source:String = ""
    try {
      val file = io.Source.fromFile(fileLocation)
      source = file.mkString
      file.close()
      return source;
    }
    catch {
      case ex: FileNotFoundException => println("File wasn't found: " + fileLocation); return ""
    }
  source;
  }
}
