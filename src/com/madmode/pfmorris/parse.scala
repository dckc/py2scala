package com.madmode.pfmorris

import com.madmode.py2scala.batteries

import com.madmode.py2scala.__builtin__._

/**
 * ###############################################################
 * //
 * //           Check both math and proof syntax
 * //
 * //###############################################################
 */
object parse {

  import batteries.sys
  import batteries.pickle
  import batteries.os
  import getpath.{ getpath }
  import batteries.stat.{ ST_MTIME }
  //
  import pattern._
  //import newsynt as synt
  import synt._
  val verbose = False

  def main(args: Array[String]): Unit = {
    if (len(args) <= 1) {
      println("No file specified.")
      throw new SystemExit()
    }
    val Arg_1 = args(1)
    var f: File = null
    try {
      f = open((Arg_1 + ".tex"), "r")
    } catch {
      case _: Throwable => {
        println((Arg_1 + ".tex not found"))
        throw new SystemExit()
      }
    }
    val line_list = f.readlines()
    f.close()
    var getprops = False
    //###########################################################
    //
    // Load math data from .dfs file
    //
    //###########################################################
    var new_dfs = False
    var syntdb: synt.MD = null
    var dfs_mtime: Long = -1
    try {
      val f = open((Arg_1 + ".dfs"), 'r')
      dfs_mtime = os.stat((Arg_1 + ".dfs"))(ST_MTIME)
      syntdb = MD.fromJson(pickle.load(f))
      f.close()
    } catch {
      case _: Throwable => {
        new_dfs = True
        syntdb = synt.makemathdb()
        getprops = True
      }
    }
    if (syntdb == null) {
      syntdb = synt.makemathdb()
      getprops = True
    }
    synt.mathdb = syntdb
    if (syntdb.MD_RSFLG) {
      dfs_mtime = -1
    }
    //############################################################
    //
    //  Do pre-processing: Run all directives and
    //
    //     merge revised .dfs files
    //
    //############################################################
    for (r <- line_list) {
      val inputfilem = pattern.inputfile.match_(r)
      if (inputfilem) {
        val fname = inputfilem.group(1)
        val dfs_file = (inputfilem.group(2) + ".dfs")
        val fpathname = getpath(fname)
        // Should perhaps require the same path
        val dfs_pathname = getpath(dfs_file)
        if (!fpathname) {
          println("File" + fname + " not found.")
          throw new SystemExit()
        }
        if (os.stat(fpathname)(ST_MTIME) > dfs_mtime) {
          for (s <- open(fpathname)) {
            if (pattern.directive.match_(s)) {
              if (inputfilem.group(2) == Arg_1) {
                //@@@@synt.process_directive(shereditary_only=False)
                throw new Exception("TODO")
              } else {
                //@@@synt.process_directive(s)
                throw new Exception("TODO")
              }
            }
          }
        }
        if (fname != "utility.tdf" && fname != "utility.ldf" && fname != (Arg_1 + ".tdf") && fname != (Arg_1 + ".ldf") && !dfs_pathname) {
          println("Warning: can't find " + dfs_file)
        }
        if (dfs_pathname && os.stat(dfs_pathname)(ST_MTIME) > dfs_mtime) {
          val f = open(dfs_pathname)
          val pickled_db = MD.fromJson(pickle.load(f))
          println("Merging " + dfs_pathname)
          synt.dbmerge(synt.mathdb, pickled_db)
          f.close()
        }
      }
    }
    /*@@@@@@@@@@@@@@@@@
  val proppathname = getpath(syntdb(synt.MD_PFILE))
  if (! proppathname) {
    println("Can't find" + syntdb(synt.MD_PFILE))
    throw new SystemExit()
    }
  if (dfs_mtime != -1 && os.stat(proppathname)(ST_MTIME) > dfs_mtime) {
    val getprops = True
    println("Warning: Properties file, " + syntdb(synt.MD_PFILE) + "has changed!")
    }
  if (getprops) {
    synt.mathdb = synt.readprops(proppathname, synt.mathdb)
    }
  for (val s <- line_list) {
    if (pattern.directive.match_(s)) {
      synt.process_directive(s)
      }
    }
  val line_list = synt.translate(line_list)
  //
  //###################################################
  //
  //  States
  //
  //  1 = Expecting Theorem or Inference Rule
  //  2 = Expecting Proof 
  //  3 = Expecting Proof Note
  //  4 = Expecting Derivation 
  //  5 = Expecting Derivation Note
  //
  //###################################################
  val state = 1
  val line_num = 1
  val linetail = List(line_list(0), 0, line_num, line_list)
  val n_definitions = 0
  val n_axioms = 0
  val n_newdefinitions = 0
  val n_unflagged_defs = 0
  val n_unnumbered_defs = 0
  val n_undefined_terms = 0
  val n_undefined_formulas = 0
  val n_inference_rules = 0
  val notenumlist = List()
  val thmnumlist = List()
  val shortproofs = List()
  val fullproofs = List()
  val infrulelist = List()
  val derivations = List()
  val definitions = List()
  val axioms = List()
  val olddefinitions = List()
  val proofs = List()
  //
  while (linetail(0)) {
    val infrulem = pattern.inference_rule.match_(linetail(0))
    val notem = pattern.note.match_(linetail(0))
    val thmnumm = pattern.thmnum.match_(linetail(0))
    val byem = pattern.bye.match_(linetail(0))
    val noparsem = pattern.noparse.match_(linetail(0))
    val TeXdollars = pattern.TeXdollar.search(linetail(0))
    if (linetail(0)(0) == '%') {
      linetail(0) = ""
      }
     else if (infrulem) {
      val linenum = linetail(2)
      infrulelist.append(linenum)
      val notenumlist = List()
      if (state == 3) {
        println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
        println("Previous proof not completed.")
        throw new SystemExit()
        }
      val getrul = synt.ruleparse(linetail(0))
      if (getrul) {
        assert(! pattern.by.match_(linetail(0)))
        }
       else {
        println("Error line " + (str(linetail(2)) + ": ") + linetail(0))
        throw new SystemExit()
        }
      val n_inference_rules = (n_inference_rules + 1)
      synt.getline(linetail)
      val state = 4
      }
     else if (thmnumm) {
      val thmnum = ((thmnumm.group(2) + '.') + thmnumm.group(3))
      if (thmnumlist.contains(thmnum)) {
        println("Repeated theorem number:" + thmnum)
        println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
        throw new SystemExit()
        }
      thmnumlist.append(thmnum)
      val notenumlist = List()
      if (state == 3) {
        println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
        println("Previous proof not completed.")
        throw new SystemExit()
        }
      linetail(0) = thmnumm.group(4)
      val getfm = synt.getformula(linetail, verbose)
      if (getfm) {
        val bym = pattern.by.match_(linetail(0))
        if (! bym) {
          synt.getline(linetail, verbose)
          //				if pattern.thmnum.match_(linetail[0]):
          //					print "Leave a line between propositions."
          //					print "Line", linetail[2],":"
          //					raise SystemExit
          val bym = pattern.by.match_(linetail(0))
          }
        if (bym) {
          val rp = synt.refparse(bym.group(1))
          if (rp == 0) {
            print("Error in reference:")
            println(bym.group(1))
            println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
            throw new SystemExit()
            }
           else if (rp == List('D')) {
            val n_definitions = (n_definitions + 1)
            if (getfm(2)(0)(0) == 51) {
              val n_newdefinitions = (n_newdefinitions + 1)
              }
             else {
              olddefinitions.append(thmnum)
              }
            val dc = synt.definition_check(getfm(0))
            if (dc) {
              println("Error in definition" + thmnum)
              println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
              println(dc)
              throw new SystemExit()
              }
            }
           else if (getfm(2)(0)(0) == 51) {
            println("Proposition" + thmnum + " is a definition. By `D' needed.")
            throw new SystemExit()
            }
           else if (rp == List('A')) {
            axioms.append(thmnum)
            val n_axioms = (n_axioms + 1)
            }
           else {
            shortproofs.append(thmnum)
            proofs.append(thmnum)
            }
          linetail(0) = ""
          val state = 1
          }
         else if (getfm(2)(0)(0) == 51) {
          val n_newdefinitions = (n_newdefinitions + 1)
          val n_unflagged_defs = (n_unflagged_defs + 1)
          definitions.append(thmnum)
          val state = 2
          }
         else {
          val state = 2
          }
        }
       else {
        println("Error line " + (str(linetail(2)) + ": ") + linetail(0))
        throw new SystemExit()
        }
      }
     else if (notem) {
      val notenum = notem.group(1)
      if (notenumlist.contains(notenum)) {
        println("Repeated note number")
        println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
        throw new SystemExit()
        }
      notenumlist.append(notenum)
      if (state == 1) {
        println("Note outside of proof")
        println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
        throw new SystemExit()
        }
      linetail(0) = notem.group(3)
      if (! synt.getnote(linetail, verbose)) {
        println("Note does not parse.")
        println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
        throw new SystemExit()
        }
      if (state == 2) {
        val state = 3
        }
       else if (state == 4) {
        val state = 5
        }
      val bym = pattern.by.match_(linetail(0))
      if (! bym) {
        synt.getline(linetail, verbose)
        val bym = pattern.by.match_(linetail(0))
        }
      if (bym) {
        if (synt.refparse(bym.group(1)) == 0) {
          print("Error in reference:")
          println(bym.group(1))
          println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
          throw new SystemExit()
          }
        linetail(0) = ""
        }
      }
     else if (byem) {
      if (state == 1 || state == 2 || state == 4) {
        println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
        println("No proof or derivation to finish.")
        throw new SystemExit()
        }
      if (synt.refparse(byem.group(1)) == 0) {
        print("Error in reference:")
        println(byem.group(1))
        println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
        throw new SystemExit()
        }
      if (state == 3) {
        fullproofs.append(thmnum)
        proofs.append(thmnum)
        }
       else if (state == 5) {
        derivations.append(linenum)
        }
      linetail(0) = ""
      val state = 1
      }
     else if (noparsem) {
      linetail(0) = ""
      }
     else if (TeXdollars) {
      linetail(0) = linetail(0).substring(TeXdollars.end(1))
      val getfm = synt.getformula(linetail, verbose)
      if (! getfm) {
        println("ABC")
        println(linetail(3)(4))
        println("Error line" + (str(linetail(2)) + ": ") + linetail(0))
        throw new SystemExit()
        }
       else if (getfm(2)(0)(0) == 51) {
        println("Warning un-numbered definition line:" + linetail(2))
        val n_unnumbered_defs = (n_unnumbered_defs + 1)
        val n_newdefinitions = (n_newdefinitions + 1)
        }
      }
     else {
      linetail(0) = ""
      }
    if (linetail(0) == "") {
      if (synt.getline(linetail, verbose) == "") {
        break
        }
      }
    }
  val save_rsflag = synt.mathdb(synt.MD_RSFLG)
  synt.mathdb(synt.MD_RSFLG) = False
  val g = open((Arg_1 + ".dfs"), 'w')
  pickle.dump(synt.mathdb, g)
  if (new_dfs) {
    println("Creating " + (Arg_1 + ".dfs"))
    }
  g.close()
  val f = open((Arg_1 + ".pfs"), 'w')
  for (val x <- proofs) {
    f.write((x + '
'))
    }
  f.close()
  if (derivations) {
    val f = open((Arg_1 + ".dvs"), 'w')
    for (val x <- derivations) {
      f.write((str(x) + '
'))
      }
    f.close()
    }
  val n_shortproofs = len(shortproofs)
  val n_fullproofs = len(fullproofs)
  val n_props = len(thmnumlist)
  val n_olddefs = len(olddefinitions)
  val n_derivations = len(derivations)
  if (n_newdefinitions == 1) {
    println()
    println("1 new definition processed.")
    }
   else if (n_newdefinitions > 1) {
    println()
    println(n_newdefinitions + " new definitions processed.")
    }
  if (((n_newdefinitions - n_unflagged_defs) - n_unnumbered_defs) == 1) {
    println("    " + "1 numbered and flagged")
    }
   else if (((n_newdefinitions - n_unflagged_defs) - n_unnumbered_defs) > 1) {
    println("    " + ((n_newdefinitions - n_unflagged_defs) - n_unnumbered_defs) + "numbered and flagged")
    }
  if (n_unnumbered_defs == 1) {
    println("    " + "1 unnumbered definition")
    }
   else if (n_unnumbered_defs > 1) {
    println("    " + n_unnumbered_defs + "unnumbered definitions")
    }
  if (n_unflagged_defs == 1) {
    print("    " + "1 unflagged definition")
    }
   else if (n_unflagged_defs > 1) {
    print("    " + n_unflagged_defs + "unflagged definitions:")
    }
  for (val x <- definitions) {
    print(x)
    }
  println()
  if (save_rsflag) {
    if (n_olddefs == 1) {
      print("Warning! ")
      print("    " + "1 definition already readable: ")
      }
     else if (n_olddefs > 1) {
      print("Warning! ")
      print("    " + n_olddefs + "definitions already readable: ")
      }
    for (val x <- olddefinitions) {
      print(x)
      }
    println()
    }
  if (n_undefined_terms == 1) {
    println("1 new undefined term")
    }
   else if (n_undefined_terms > 1) {
    println(n_undefined_terms + "new undefined terms")
    }
  if (n_undefined_formulas == 1) {
    println("1 new undefined formula")
    }
   else if (n_undefined_formulas > 1) {
    println(n_undefined_formulas + "new undefined formulas")
    }
  if (n_props == 1) {
    println("1 numbered proposition:")
    }
   else if (n_props > 1) {
    println(n_props + "numbered propositions:")
    }
  if (n_definitions == 1) {
    println("     1 definition")
    }
   else if (n_definitions > 1) {
    println("    " + n_definitions + "definitions")
    }
  if ((n_shortproofs + n_fullproofs) == 1) {
    println("     1 proof")
    }
   else if ((n_shortproofs + n_fullproofs) > 1) {
    println("    " + (n_shortproofs + n_fullproofs) + "proofs")
    }
  val n_unproved = (((n_props - n_shortproofs) - n_fullproofs) - n_definitions)
  if (n_unproved == 1) {
    println("    " + "1 proposition given without proof")
    }
   else if (n_unproved > 1) {
    println("    " + n_unproved + "propositions given without proof")
    }
  if (n_axioms == 1) {
    println("    " + "1 axiom included")
    }
   else if (n_axioms > 1) {
    println("    " + n_axioms + "axioms included")
    }
  if (n_inference_rules == 1) {
    println()
    println("1 rule of inference")
    }
   else if (n_inference_rules > 1) {
    println()
    println(n_inference_rules + "rules of inference")
    }
  if (n_derivations == 1) {
    println("     1 derivation")
    }
   else if (n_derivations > 1) {
    println("    " + n_derivations + "derivations")
    }
    * @@@@@@@@@@@@
    */
  }
}
