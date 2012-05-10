package com.noimawesome.scalacc

import parser.CParser
import tools.CommandLineArgs

/**
 * Created with IntelliJ IDEA.
 * User: mthorpe
 * Date: 21/04/12
 * Time: 16:59
 * To change this template use File | Settings | File Templates.
 */

object Compiler extends App {
  println("Starting now")
  val arguments = new CommandLineArgs(args)
  arguments.cFiles foreach (new CParser(_))
}