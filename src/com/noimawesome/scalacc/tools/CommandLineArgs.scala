package com.noimawesome.scalacc.tools

/**
 * Created with IntelliJ IDEA.
 * User: mthorpe
 * Date: 21/04/12
 * Time: 17:50
 * To change this template use File | Settings | File Templates.
 */

class CommandLineArgs(args: Array[String]) {
  val parseOnly: Boolean = args.contains("--parse")
  // do nothing is for testing only
  val doNothing: Boolean = args.contains("--nothing")
  val cFiles: Array[String] = args filter (x => (x.endsWith(".c") || x.endsWith(".h")))
}
