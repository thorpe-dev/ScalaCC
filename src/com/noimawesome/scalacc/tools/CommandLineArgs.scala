package com.noimawesome.scalacc.tools

class CommandLineArgs(args: Array[String]) {
  val parseOnly: Boolean = args.contains("--parse")
  // do nothing is for testing only
  val doNothing: Boolean = args.contains("--nothing")
  val cFiles: Array[String] = args filter (x => (x.endsWith(".c") || x.endsWith(".h")))
}
