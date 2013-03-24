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

s = '\A\\\[qw]\^\{(\d+)\}_\{(\d+)\}'
newschem = compile(s)

s = '\A\\\([pqr]+)var'
gensent = compile(s)

s = '\A\\\([pqr]+|[uvw]+)bar(p*)'
genschem = compile(s)

"""

	  /*
# A Token consists of 
#
#Either:
#    1. a sequence of digits
#OR
#    2. One of the following punctuation marks:
#         .  <  >  ;  /  :  [  ]  (  +  )  =  -  * ,
#OR
#    3. One of these slashed TeX symbols:
#         \{  \}  \.   \_  \&  \%  \# \, \> \; \!
#OR
#    4. Either:
#            a.  A single letter
#OR 
#            b.  A pair of braces { } enclosing non-brace characters
#OR
#            c.  An alphabetic control sequence, a backslash followed by letters
#
#       optionally followed by
#            d.  A prime sequence, a backslash followed by a sequence of p's not
#                followed by a letter
#
#       optionally followed by any number of sequences consisting of:
#            e.  A TeX superscript ^ or subscript _
#      
#            followed by
#            Either
#                i) a pair of braces { } enclosing non-brace characters
#            OR 
#                ii) an alphabetic control sequence, a backslash followed by letters
#            OR
#                iii) any single non-slash character
*/

	  	val token = """(\s*)(\d+|[\.<>;/:\[\]\(\+\)=\-\*\,]|\\[\{\._\}\&\%\,<;\!]|([A-Za-z]|\{[^\{]*\}|\\[A-Za-z]+)(?:\\p+(?![A-Za-z]))?(?:[\^_](?:\{[^\{]*\}|\\[A-Za-z]+|[^\\]))*)(\s*)""".r

//# token = pattern.token.match.group(2) 
//# variable stripped of decoration = pattern.token.match.group(3)

//s = r"\A\s*(\$[^\$]?\$\s*[;:\(\)\+\-]*\s*)*\\C\s*(\$[^\$]+\$)\s*\Z"
//s = r"\A\s*(\$[^\$]+\$\s*,?\s*)*\\C\s*(\$[^\$]+\$)\s*\Z"
	val inference_rule = """\A\s*((\$[^\$]+\$\s*)|([HU\;\:\(\)\+\-\,]+\s*))*\\C\s*(\$[^\$]+\$)\s*\Z""".r


/*
#These are not accepted as tokens yet so they do not work:
s = r"(\\(bigl|Bigl|biggl|Biggl|left))?(\(|\[|\\lfoor|\\lceil|\\langle)"
TeX_leftdelimiter = compile(s) 
s = r"(\\(bigr|Bigr|biggr|Biggr|right))?(\(|\[|\\rfoor|\\rceil|\\rangle)"
TeX_rightdelimiter = compile(s)
*/

	val SKIP2 = """
ignore_token = compile(r"\A\\,|\\>|\\;|\\!\Z")

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
				/*@@
				s = token.match(repeat)
				if s {
					print s.group(2)
					print s.groups()
				}
				* 
				*/
			}
	}

}