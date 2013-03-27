package com.madmode.pfmorris

/**
 * ########################################
 * # pattern.py
 * #
 */
object pattern {
  import __builtin__._
  import batteries._

  import re.{compile}

  //s = '\A\s*\\\\(chapter|(sub)*section)\W.*'
  //latex_major_unit = compile(s)
  var s = """\\mathchardef\\([A-Z,a-z]*)="([A-F,\d]*)"""
  val chardef = compile(s)
  s = """\A\s*\\chap\W+(\d+)\..*"""
  val chapthead = compile(s)
  s = """\A\s*\\section\W.*"""
  val latex_section = compile(s)
  val latex_unit_prefix = """\A\s*\\"""
  val latex_unit_suffix = """\W.*"""
  // The alternate spellings of 'prop' have been removed
  s = """\A\\(prop)\W+(\d+)\.(\d+)\s*\$(.*)"""
  val thmnum = compile(s)
  val inputfile = compile("""\s*\\input\s+((.+)\.([lt]df))""")
  //directive = compile('\s*%([:_A-Za-z]+)\s+((\S*)\s*(\S*)\s*(\S*)\s*(\S*)\s*(\S*)\s*)')
  val directive = compile("""\s*%([:_A-Za-z]+)\s+((\S*)\s*(.*?))\s*\Z""")
  s = """\A\s*\\line([a-z])\s*\$(.*)"""
  val line = compile(s)
  s = """\A[^\$\%]*\\By(\W.*)"""
  val by = compile(s)
  s = """\A[^\$\%]*\\Bye(\W.*)"""
  val bye = compile(s)
  // Thanks to Karl Berry for enabling this:
  s = """\A[^\%]*\\note\W+(\d+)\s+([^\$]*\$)(.*)"""
  val note = compile(s)
  s = """(?<!\\)(%)|(\\noparse)""" // @@?P<TeXcomment>
  val Noparse = compile(s)
  s = """(?<!\\)(%)"""
  val TeXcomment = compile(s)
  s = """(?<!\\)(\$+)"""
  var TeXdollar = compile(s)
  s = """(?<!\\)(\$+)"""
  TeXdollar = compile(s)
  val blankline = compile("""(\s*)(\$+)""")
  val ref = compile("""\A((\d*)(\.\d+)+)([a-z]*)(.*)""")
  val outfileref = compile("""\A((\d+)(\.\d+)+)([a-z]+)(.*)""")
  val propref = compile("""(\A|\D)((\d+)((\.\d+)+))(\Z|[^\.a-z0-9])""")
  s = """(\A|.*\D)\.(\d+)(.*)"""
  val noteref = compile(s)
  s = """\A\$([^\$]*)\$(.*)"""
  val TeXmath = compile(s)
  s = """\A[^\$]*\\noparse.*"""
  val noparse = compile(s)
  s = """z_\{(\d+)\}"""
  val bvar = compile(s)
  s = """\A\\[qw]\^\{(\d+)\}_\{(\d+)\}"""
  val newschem = compile(s)
  s = """\A\\([pqr]+)var"""
  val gensent = compile(s)
  s = """\A\\([pqr]+|[uvw]+)bar(p*)"""
  val genschem = compile(s)
  // A Token consists of 
  //
  //Either:
  //    1. a sequence of digits
  //OR
  //    2. One of the following punctuation marks:
  //         .  <  >  ;  /  :  [  ]  (  +  )  =  -  * ,
  //OR
  //    3. One of these slashed TeX symbols:
  //         \{  \}  \.   \_  \&  \%  \# \, \> \; \!
  //OR
  //    4. Either:
  //            a.  A single letter
  //OR 
  //            b.  A pair of braces { } enclosing non-brace characters
  //OR
  //            c.  An alphabetic control sequence, a backslash followed by letters
  //
  //       optionally followed by
  //            d.  A prime sequence, a backslash followed by a sequence of p's not
  //                followed by a letter
  //
  //       optionally followed by any number of sequences consisting of:
  //            e.  A TeX superscript ^ or subscript _
  //      
  //            followed by
  //            Either
  //                i) a pair of braces { } enclosing non-brace characters
  //            OR 
  //                ii) an alphabetic control sequence, a backslash followed by letters
  //            OR
  //                iii) any single non-slash character
  s = """(\s*)(\d+|[\.<>;/:\[\]\(\+\)=\-\*\,]|\\[\{\._\}\&\%\,<;\!]|([A-Za-z]|\{[^\{]*\}|\\[A-Za-z]+)(?:\\p+(?![A-Za-z]))?(?:[\^_](?:\{[^\{]*\}|\\[A-Za-z]+|[^\\]))*)(\s*)"""
  val token = compile(s)
  // token = pattern.token.match.group(2) 
  // variable stripped of decoration = pattern.token.match.group(3)
  s = """\A\s*(\$[^\$]?\$\s*[;:\(\)\+\-]*\s*)*\\C\s*(\$[^\$]+\$)\s*\Z"""
  s = """\A\s*(\$[^\$]+\$\s*,?\s*)*\\C\s*(\$[^\$]+\$)\s*\Z"""
  s = """\A\s*((\$[^\$]+\$\s*)|([HU\;\:\(\)\+\-\,]+\s*))*\\C\s*(\$[^\$]+\$)\s*\Z"""
  val inference_rule = compile(s)
  //These are not accepted as tokens yet so they do not work:
  s = """(\\(bigl|Bigl|biggl|Biggl|left))?(\(|\[|\\lfoor|\\lceil|\\langle)"""
  val TeX_leftdelimiter = compile(s)
  s = """(\\(bigr|Bigr|biggr|Biggr|right))?(\(|\[|\\rfoor|\\rceil|\\rangle)"""
  val TeX_rightdelimiter = compile(s)
  val ignore_token = compile("""\A\\,|\\>|\\;|\\!\Z""")
  s = """\A([^\d\s\.\$]*)(.*)"""
  val puncts = compile(s)
  s = """\A(\d+)(.*)"""
  val nums = compile(s)
  s = """\A[^\$]*\$(.*)"""
  val dollar = compile(s)
  s = """\A([^,\)\(]*)([,\)\(]+.*)"""
  val findsingle = compile(s)
  val skipstring = "mskip 5mu plus 5mu"
  s = """(?<!\\)\\((mskip 5mu plus 5mu)|([A-Za-z]+))"""
  val alphacontrolseq_or_skip = compile(s)

  def main(args: Array[String]): Unit = {
    import __builtin__._
    var repeat = "yes"
    while (repeat) {
      repeat = raw_input("Enter possible token string: ")
      val s = token.match_(repeat)
      if (s) {
        println(s.group(2))
        println(s.groups())
      }
    }
  }

  def main_x(args: Array[String]): Unit = {

    var repeat = "yes"
    while (repeat.length > 0) {
      repeat = raw_input("Enter possible token string: ")
      token.findFirstIn(repeat) match {
        case Some(token(s, x, y)) => {
          println(x)
          println((x, y))
        }
        case Some(oops) => {
          println("oops!" + oops)
        }
        case None => ()
      }
    }
  }

}
