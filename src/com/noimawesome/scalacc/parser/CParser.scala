package com.noimawesome.scalacc.parser

import java.io.FileNotFoundException

/**
 * Created with IntelliJ IDEA.
 * User: mthorpe
 * Date: 05/05/12
 * Time: 18:26
 * To change this template use File | Settings | File Templates.
 */

class CParser(fileLocation: String) {
  var source: String = openFile()

  print ("Can be printed = " + CheckCSyntax.parse(source))


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
  return source;
  }
}
