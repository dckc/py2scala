/**
########################################
# pattern.py
#
*/

package com.madmode.pfmorris

import scala.util.matching.Regex

object pattern {

	//#s = '\A\s*\\\\(chapter|(sub)*section)\W.*'
	//#latex_major_unit = compile(s)

	val chardef = """\\mathchardef\\([A-Z,a-z]*)="([A-F,\d]*)""".r

	val chapthead = """\A\s*\\chap\W+(\d+)\..*""".r

	val latex_section = """\A\s*\\section\W.*""".r

	val SKIP = """
latex_unit_prefix = '\A\s*\\\\'
latex_unit_suffix = '\W.*'

# The alternate spellings of 'prop' have been removed
s = '\A\\\\(prop)\W+(\d+)\.(\d+)\s*\$(.*)'
thmnum = compile(s)

inputfile = compile('\s*\\\\input\s+((.+)\.([lt]df))')

#directive = compile('\s*%([:_A-Za-z]+)\s+((\S*)\s*(\S*)\s*(\S*)\s*(\S*)\s*(\S*)\s*)')
directive = compile('\s*%([:_A-Za-z]+)\s+((\S*)\s*(.*?))\s*\Z')

s = '\A\s*\\\\line([a-z])\s*\$(.*)'
line = compile(s)

s ='\A[^\$\%]*\\\\By(\W.*)'
by = compile(s)

s ='\A[^\$\%]*\\\\Bye(\W.*)'
bye = compile(s)

# Thanks to Karl Berry for enabling this:
s = '\A[^\%]*\\\\note\W+(\d+)\s+([^\$]*\$)(.*)'
note = compile(s)

s = '(?<!\\\\)(?P<TeXcomment>%)|(\\\\noparse)'
Noparse = compile(s)

s = '(?<!\\\\)(%)'
TeXcomment = compile(s)

s = '(?<!\\\\)(\$+)'
TeXdollar = compile(s)
s = r'(?<!\\)(\$+)'
TeXdollar = compile(s)

blankline = compile(r'(\s*)(\$+)')

ref=compile('\A((\d*)(\.\d+)+)([a-z]*)(.*)')

outfileref = compile('\A((\d+)(\.\d+)+)([a-z]+)(.*)')

propref = compile("(\A|\D)((\d+)((\.\d+)+))(\Z|[^\.a-z0-9])")

s = '(\A|.*\D)\.(\d+)(.*)'
noteref = compile(s)

s = '\A\$([^\$]*)\$(.*)'
TeXmath = compile(s)

s = '\A[^\$]*\\\\noparse.*'
noparse = compile(s)

s = 'z_\{(\d+)\}'
bvar = compile(s)
"""

	val newschem = """\A\\\[qw]\^\{(\d+)\}_\{(\d+)\}""".r

	val gensent = """\A\\\([pqr]+)var""".r

	val genschem = """\A\\\([pqr]+|[uvw]+)bar(p*)""".r

	val token = """(?x)(\s*)
# A Token consists of 
#
	  	  (
#Either:
#    1. a sequence of digits
	  	  \d+|
#OR
#    2. One of the following punctuation marks:
#         .  <  >  ;  /  :  [  ]  (  +  )  =  -  * ,
	  	  [\.<>;/:\[\]\(\+\)=\-\*\,]|
#OR
#    3. One of these slashed TeX symbols:
#         \{  \}  \.   \_  \&  \%  \# \, \> \; \!
	  	  \\[\{\._\}\&\%\,<;\!]|
#OR
#    4. Either:
#            a.  A single letter
	  	  ([A-Za-z]|
#OR 
#            b.  A pair of braces { } enclosing non-brace characters
	  	  \{[^\{]*\}|
#OR
#            c.  An alphabetic control sequence, a backslash followed by letters
	  	  \\[A-Za-z]+)
#
#       optionally followed by
#            d.  A prime sequence, a backslash followed by a sequence of p's not
#                followed by a letter
	  	  (?:\\p+(?![A-Za-z]))?
#
#       optionally followed by any number of sequences consisting of:
#            e.  A TeX superscript ^ or subscript _
	  	  (?:[\^_]
#      
#            followed by
#            Either
#                i) a pair of braces { } enclosing non-brace characters
	  	  (?:\{[^\{]*\}|
#            OR 
#                ii) an alphabetic control sequence, a backslash followed by letters
	  	  \\[A-Za-z]+|
#            OR
#                iii) any single non-slash character
	  	  [^\\]))*)
	  	  (?:\s*)""".r

//# token = pattern.token.match.group(2) 
//# variable stripped of decoration = pattern.token.match.group(3)

//s = r"\A\s*(\$[^\$]?\$\s*[;:\(\)\+\-]*\s*)*\\C\s*(\$[^\$]+\$)\s*\Z"
//s = r"\A\s*(\$[^\$]+\$\s*,?\s*)*\\C\s*(\$[^\$]+\$)\s*\Z"
	val inference_rule = """\A\s*((\$[^\$]+\$\s*)|([HU\;\:\(\)\+\-\,]+\s*))*\\C\s*(\$[^\$]+\$)\s*\Z""".r


	/* These are not accepted as tokens yet so they do not work: */
	val TeX_leftdelimiter = """(\\(bigl|Bigl|biggl|Biggl|left))?(\(|\[|\\lfoor|\\lceil|\\langle)""".r
	val TeX_rightdelimiter = """(\\(bigr|Bigr|biggr|Biggr|right))?(\(|\[|\\rfoor|\\rceil|\\rangle)""".r

	val ignore_token = """\A\\,|\\>|\\;|\\!\Z""".r

	val SKIP2 = """

s = "\A([^\d\s\.\$]*)(.*)"
puncts = compile(s)

s = "\A(\d+)(.*)"
nums = compile(s)

s = "\A[^\$]*\$(.*)"
dollar = compile(s)

s = "\A([^,\)\(]*)([,\)\(]+.*)"
findsingle = compile(s)

skipstring = "mskip 5mu plus 5mu"

s = '(?<!\\\\)\\\\((mskip 5mu plus 5mu)|([A-Za-z]+))'
alphacontrolseq_or_skip = compile(s)
""".r

	def raw_input(prompt: String): String = {
	  import java.io.{BufferedReader, InputStreamReader}
	  val b = new BufferedReader(new InputStreamReader(System.in))
	  System.out.print(prompt)
	  return b.readLine()
	}
	
	def main(args: Array[String]): Unit = {
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