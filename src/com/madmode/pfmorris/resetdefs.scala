package com.madmode.py2scala

import __fileinfo__._
import com.madmode.py2scala.__builtin__._
import com.madmode.py2scala.batteries._

object resetdefs {
  //!/usr/bin/python
  //###############################################################
  //
  //   Reset definitions in the .dfs file to readiness to 
  //	 run the argument file as if for the first time. 
  //
  //###############################################################
  //
  // Command Line Argument:
  //   1. The name of the file whose definitions are to be reset 
  //      (without the .tex extension)
  //
  //###############################################################
  import sys
  import pickle
  //
  import synt
  import pattern
  //
  //
  //
  //
  if (len(sys.argv) < 2) {
    println("Usage: resetdefs <file> ")
    throw new SystemExit()
    }
  val filename = (sys.argv(1) + ".tex")
  try {
    val f = open(filename, "r")
    }
  catch {
    case _ => {
      println((filename + " not found."))
      throw new SystemExit()
      }
    }
  val r = f.readlines()
  f.close()
  val tdf_filelist = List()
  for (s <- r) {
    val inputfilem = pattern.inputfile.match(s)
    if (inputfilem) {
      tdf_filelist.append(inputfilem.group(2))
      }
    }
  if (tdf_filelist.contains(sys.argv(1))) {
    tdf_filelist.remove(sys.argv(1))
    }
  if (tdf_filelist.contains("utility")) {
    tdf_filelist.remove("utility")
    }
  if (tdf_filelist) {
    print("Included files:")
    for (x <- tdf_filelist) {
      print(x)
      }
    println()
    }
  val mathdb = synt.makemathdb()
  val filename = (sys.argv(1) + ".dfs")
  try {
    val f = open(filename, "r")
    val x = pickle.load(f)
    f.close()
    val mathdb(synt.MD_TROPS) = x(synt.MD_TROPS)
    val mathdb(synt.MD_TRMUL) = x(synt.MD_TRMUL)
    val mathdb(synt.MD_CAOPS) = x(synt.MD_CAOPS)
    val mathdb(synt.MD_THMS) = x(synt.MD_THMS)
    }
  catch {
    case _ => {
      println("Creating " + filename)
      }
    }
  println(len(mathdb))
  for (tdf <- tdf_filelist) {
    val g = open((tdf + ".dfs"))
    val db = pickle.load(g)
    g.close()
    if (len(db) < len(mathdb)) {
      println("len " + tdf + " = " + len(db))
      throw new SystemExit()
      }
    synt.dbmerge(mathdb, db)
    }
  val f = open((sys.argv(1) + ".dfs"), "w")
  pickle.dump(mathdb, f)
  f.close()
  }

object __fileinfo__ {
  val __name__ = "com.madmode.py2scala.resetdefs"
}
