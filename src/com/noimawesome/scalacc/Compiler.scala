package com.noimawesome.scalacc

import parser.CParser
import tools.CommandLineArgs

object Compiler extends App {
  println("Starting now")
  val arguments = new CommandLineArgs(args)
  arguments.cFiles foreach (new CParser(_))
}