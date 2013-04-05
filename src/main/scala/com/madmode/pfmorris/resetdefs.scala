package com.madmode.pfmorris
// implicit:
//  import synt
//  import pattern

import scala.collection.mutable.{ ArrayBuffer => PyList }
import scala.collection.generic.FilterMonadic

import __fileinfo__._
import com.madmode.py2scala.__builtin__._
import com.madmode.py2scala.{ batteries => py }

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
  //import py.sys argv is passed to main rather than via sys
  import py.pickle
  //
  //
  //
  //
  //
  def with_input(argv: Seq[String])(thunk: (String, File) => Unit) = {
    if (len(argv) < 2) {
      println("Usage: resetdefs <file> ")
      throw new SystemExit()
    }
    val filename = (argv(1) + ".tex")
    val f = try {
      open(filename, "r")
    } catch {
      case _: Throwable => {
        println((filename + " not found."))
        throw new SystemExit()
      }
    }
    thunk(argv(1), f)
    f.close
  }

  def with_filelist(f: File, already: String)(thunk: PyList[String] => Unit) = {
    import py.re._ // for Match -> Boolean coercion
    val r = f.readlines()
    f.close()

    val tdf_filelist = PyList[String]()
    for (s <- r if s != already && s != "utility") {
      val inputfilem = pattern.inputfile.match_(s)
      if (inputfilem) {
        tdf_filelist += inputfilem.group(2)
      }
    }
    if (tdf_filelist) {
      print("Included files:")
      for (x <- tdf_filelist) {
        print(x)
      }
      println()
    }
    tdf_filelist
  }

  def main(argv: Array[String]): Unit = {
    with_input(argv) {
      case (infn, inf) => {
        with_filelist(inf, infn) {
          case tdf_filelist => {
            val mathdb = synt.makemathdb()
            val filename = (argv(1) + ".dfs")
            try {
              val f = open(filename, "r")
              val x = synt.MD.load(f)
              f.close()
              val mathdb.MD_TROPS = x.MD_TROPS
              val mathdb.MD_TRMUL = x.MD_TRMUL
              val mathdb.MD_CAOPS = x.MD_CAOPS
              val mathdb.MD_THMS = x.MD_THMS
            } catch {
              case _: Throwable => {
                println("Creating " + filename)
              }
            }
            println("@@len(mathdb)")
            for (tdf <- tdf_filelist) {
              val g = open((tdf + ".dfs"))
              val db = synt.MD.load(g)
              g.close()
              if (False /*@@len(db) < len(mathdb)*/ ) {
                println("len " + tdf + " =  + @@len(db)")
                throw new SystemExit()
              }
              synt.dbmerge(mathdb, db)
            }
            val f = open((argv(1) + ".dfs"), "w")
            pickle.dump(mathdb, f)
            f.close()
          }
        }
      }
    }
  }
}

object __fileinfo__ {
  val __name__ = "com.madmode.py2scala.resetdefs"
}
