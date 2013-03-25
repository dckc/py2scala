/*
################################################################
#
#           Code for various parsing applications including
#           code for parsing one term or formula 
#
################################################################
*/

package com.madmode.pfmorris

object synt {

  /**
   * Each parse begins with a header.  Each header begins with a 
   * tag which is one of the following numbers.
   */
  object Tag extends Enumeration {
    type Tag = Value
   /* Tags which appear only at the top level of a complete parse: */
    val Term_Definor,
    Formula_Definor,
    Connector,
    _four,
    Unrecognized_constant,
    Right_Parenthesis,
    Other_known_constant,
    Term_Seeking_Notarian,
    Formula_Seeking_Notarian, 

    Variable,
    Sentence_variable, // 0-ary schemator
    Function_symbol,   //n-ary schemator n > 0
    Predicate_symbol,  // n-ary schemator n > 0
    Noun,              // (Length 1 constant term) 
    Boolean_Constant,   // (Length 1 constant formula)

    Left_Parenthesis,
    Introductor,      
    Decimal_Numeration,
    Left_Scope_Bracket,
    Right_Scope_Bracket,
    Colon, 
    Semi_colon,
    Ignore_Token,  // (Not parsed)

    /* Tags which may appear at any level of a complete parse: */
    Term,          // (Not length 1) 
    Formula,       // (Not length 1) 
    Schematic_Term,        
    Schematic_Formula, // (Not zero-ary) 
    Parade_Term,
    Parade_Formula,
    Newly_Defined_Form,
    Scope, 
    Undefined_Expression_2, // (2nd level only)  
    Undefined_Parade,     // (3rd level only)  
    New_Definition,

    /* Tags which may appear only at the beginning of an incomplete parse: */
    Non_Parenthetical_Expression, // -1
    Parenthetical_Expression,
    Undefined_Expression,
    Schematic_Term_neg,
    Schematic_Formula_neg,
    Term_Quantifying_Form,
    Formula_Quantifying_Form,
    Scope_neg,
    Parade,
    _neg_ten,
    New_Definition_neg,
    Undefined_Parade_neg = Value;
  }
  import Tag._
  
  /**
   *  This database is loaded from the dfs file.  It is modified by parse and mathparse
   *  but not by check.  It is initialized by resetdefs and resetprops.  
   */
	var mathdb: MathDB = null;

  	import scala.collection.mutable.{Map, ArrayBuffer}
  	
  	type Defienda = String
  	type Theorem = String
  	type ExtDef = String
  	type MacDef = String
 
	case class MathDB(
		var MD_SYMTYPE: Map[String, Tag],     //  Dictionary storing the type of each symbol
		var MD_PRECED: Map[String, Int],      //  Dictionary storing the precedence of each connector
		var MD_DEFS: Map[String, List[Defienda]],  //  Dictionary storing a list of definienda for each introductor
		var MD_ARITY: Map[String, Int],       //  Dictionary storing the arity of each schemator 
		var MD_TROPS: List[String],           //  List of Transitive operators
		var MD_TRMUL: Map[(String, String), String], //  Dictionary mapping 2-tuples of transitive ops to a transitive op
		var MD_CAOPS: List[String],           //  List of commutative associative ops
		var MD_THMS: List[Theorem],           //  List of all theorems from some properties file
		var MD_REFD: Map[String, ExtDef],     //  Dictionary of external file reference definitions 
		var MD_MACR: Map[String, MacDef],     //  Dictionary of user macro definitions
		var MD_RSFLG: Boolean,                //  Boolean reset definitions flag
		var MD_PFILE: String,                 //  Name of properties file
		var MD_RFILE: String                  //  Name of rules of inference file
		//var MD_LEN: Int                       //  Length of mathdb
			)

	/**
	 * The following fast moving variable keeps track of which numbers have been
	 * used already as variable tags.   
	 */
	var newvarnum = 0

	/* The following are tokens added so that TeX spacing characters can be used.*/
	val ignore_tokens = List("""\,""", """\>""", """\;""", """\!""")

	val reference_punctuator_list = List(",", ";", "<=",
	  "\\C", "G", "H", "!", "D", "P", "A", "S", "U",
	  ")", "(", "),", ",(", ";(",
	  "+", "-", "\\char124", ":", "|")

	def makemathdb() = {
	  val td = Map(
			  "\\dft" -> Term_Definor,
			  "\\dff" -> Formula_Definor,
			  ")" -> Right_Parenthesis,
			  "(" -> Left_Parenthesis,
			  "\\ls" -> Left_Scope_Bracket,
			  "\\rs" -> Right_Scope_Bracket,
			  ":" -> Colon,
			  ";" -> Semi_colon,
			  "\\}" -> Other_known_constant,
			  "\\false" -> Boolean_Constant,
			  "\\true" -> Boolean_Constant,
			  "\\Nul" -> Noun,
			  "0" -> Noun,
			  "1" -> Noun,
			  "2" -> Noun,
			  "3" -> Noun,
			  "4" -> Noun,
			  "5" -> Noun,
			  "6" -> Noun,
			  "7" -> Noun,
			  "8" -> Noun,
			  "9" -> Noun,
			  "\\ten" -> Noun
	      )

/*
###############################
#  
#   Connector Symbols
#
############################### 
#
#  The number introducing each connector list is the
#  precedence value.  
#
###############################
*/
	val connectors = List(
	(1, List("\\case")),
	(2, List("\\c")),
	(3, List("\\cond", "\\els")),
	(4, List("\\Iff")),
	(5, List("\\And", "\\Or")),
	(6, List("=", "\\ne", "\\le", "\\ge", "\\notin", "\\noti", "\\ident", "\\in", "<", ">",
	    "\\i", "\\j", "\\subset", ",")),
	(9, List("+", "-")),
	(13, List("\\cdot", "/")))
	
	val precedence = Map() ++ (for((prec, plist) <- connectors;
						  		    sym <- plist) yield (sym -> prec))
	val defs = Map()
	val arity = Map()

	for((prec, plist) <- connectors;
		 sym <- plist) {
	  td(sym) = Connector
	}
	
	td("\\ident") = Term_Definor
	td("\\Iff") = Formula_Definor
	precedence("\\dff") = 4
	precedence("\\dft") = 6
	
	/*
#######################################################
#
#  Initialize Transitive Properties
#
#######################################################
*/
	val transitive_ops = List("\\ident", "=", "\\Iff")

	val trans_mult = Map()

	/*
#######################################################
#
#  Initialize Commutative Associative Properties
#
#######################################################
*/
	val commutative_associative_ops = List()

	val theorems = List()

	val reference_dictionary = Map[String, String]()
	reference_dictionary("0") = "common.tex"

	val user_dictionary = Map()

	val reset_flag = true

	val properties_file = "properties.tex"

	val rules_file = "rules.tex"

	new MathDB(td, precedence, Map(), Map(), transitive_ops, Map(),
			   commutative_associative_ops, theorems, reference_dictionary, 
			   Map(), reset_flag, properties_file, rules_file)
	}


def dbmerge(mathdb: MathDB, db: MathDB) = {
	mathdb.MD_SYMTYPE ++= db.MD_SYMTYPE
	mathdb.MD_PRECED ++= db.MD_PRECED
	mathdb.MD_DEFS ++= db.MD_DEFS
	mathdb.MD_ARITY ++= db.MD_ARITY
	if (db.MD_REFD != null) {
		mathdb.MD_REFD ++= db.MD_REFD
	} else {
		println("Warning: format does not match.")
	}
	if (db.MD_MACR != null) {
		mathdb.MD_MACR ++= db.MD_MACR
	} else {	
		println("Warning: format does not match.")
	}
}

/*

def symtype(token):
	td = mathdb[MD_SYMTYPE]
	arity = mathdb[MD_ARITY]
	assert type(token) is str
	retval = td.get(token, 0)
	if retval:
		return retval
	else:
		if pattern.ignore_token.match(token):
			td[token] = 23
			return 23
		if pattern.TeX_leftdelimiter.match(token):
			return 6
		if pattern.TeX_rightdelimiter.match(token):
			return 16
		b = validschemator(token)
		if b:
			td[token] = b[0]
			arity[token] = b[1]
			retval = b[0]
		elif validvar(token):
			td[token] = 10
			retval = 10
		elif validnum(token):
			td[token] = 18
			retval = 18
		else:
			td[token] = 5
			retval = 5
		return retval 
	
def validschemator(token):
	schemm = pattern.newschem.match(token)
	if schemm:
		arity = int(schemm.group(1))
		if token[1] == 'w':
			return (12, arity)
		elif arity == 0:
			return (11, arity)
		else:
			return (13, arity)
 	genschemm = pattern.genschem.match(token)
	if genschemm:
		arity = len(genschemm.group(2)) + 1
		sym2 = genschemm.group(1)[0]
		if sym2 in ['p','q','r']:
			return ( 13, arity)
 		else:
			return ( 12, arity)
 	gensentm = pattern.gensent.match(token)
	if gensentm:
		return (11, 0)
	if len(token) < 5:
		return 0
	if token[0] != '\\':
		return 0
	if token[1] not in ['o','p','q','r','s','t','u','v','w']:
		return 0
	if len(token) == 6 and token[5] != 'p':
		if token[3:6] != 'var':
			return 0
		if token[1] not in ['p','q','r','s','t']:
			return 0
		if token[2] not in ['p','q','r','s','t']:
			return 0
		return (11,0)
	if token[3:5] != 'ar' :
		return 0
	if token[2] == 'v':
		if token[1] in ['o','p','q','r','s','t']and len(token) == 5:
			return (11,0)
		else:
			return 0
	elif token[2] != 'b':
		return 0
	if token[1] not in ['p','q','r','u','v','w']:
		return 0
	tail = token[5:]
	lt = len(tail)
	if tail.count('p') == lt:
		if token[1] in ['p','q','r']:
			return(13, 1+lt)
		else:
			return (12, 1+lt)
	else:
		return 0

def validvar(token):
	if token[0].isalpha() :
		return True
	elif token[0] == '{':
		x = token[1:-1]
		x = x.strip()
		if x.isalpha():
			return True
		elif x[0] == '\\':
			y = x.find(' ')
			if y == -1:
				return False
			elif x[1:y] == 'cal' and x[y:].strip().isalpha():
				return True
			else:
				return False
		else:
			return False
	else:
		tokenm = pattern.token.match(token)
		if tokenm.group(3) in allowed_variables:
			return True
		return False

allowed_variables = ['\\alpha','\\beta','\\gamma','\\delta','\\epsilon',
'\\varepsilon','\\zeta','\\eta','\\theta','\\vartheta','\\iota','\\kappa',
'\\lambda','\\mu','\\nu','\\xi','\\pi','\\varpi','\\rho','\\varrho','\\sigma',
'\\varsigma','\\tau','\\upsilon','\\phi','\\varphi','\\chi','\\psi',
'\\Gamma','\\Delta','\\Theta','\\Lambda','\\Xi','\\Pi','\\Sigma','\\Upsilon','\\Phi',
'\\Psi','\\imath','\\jmath','\\ell']

def validnum(token):
	return token.isdigit()

	
def tokenparse(token):
#
	precedence = mathdb[MD_PRECED]
	defs = mathdb[MD_DEFS]
	arity = mathdb[MD_ARITY]
	n = symtype(token)
	if n == 18:
		x = decimalparse(token)
	elif n == 12:
		x = [[-4, arity[token]], [[n], token ]]
	elif n == 13:
		x = [[-5, arity[token]], [[n], token ]]
	elif n == 16:
		x = [[-2 , []], [[n], token ]]
	elif n == 17 :
		x = [[-1, defs[token]], [[n], token ]]
	elif n == 8:
		x = [[-6, defs[token]], [[n], token ]]
	elif n == 9:
		x = [[-7, defs[token]], [[n], token ]]
	elif n in [1,2,3] :
		x = [[n], token, precedence[token]]
	else:
		# Even unknown constants are passed on as complete 
		x = [[n], token]
	return x

def decimalparse(token):
	precedence = mathdb[MD_PRECED]
	n = len(token)
	if n == 1:
		return [14, token]
	retval = token[0]
	for k in range(1,n):
		retval = [[44,precedence['+']], 
                [[44, precedence['\\cdot']], retval, '\\cdot', '\\ten'], '+', token[k]]
	return [[40, []], '(' , retval, ')' ]


def addtoken(tree, token):
	if symtype(token) == 23:
		return 1
	else:
		return addnode(tree, tokenparse(token))

def addnode(tree, item):
#tree is a list which has one entry for each pending incomplete parse tree
#item is one complete parse
#
	header = item[0]
	syntype = header[0]
	okval = 1

# An unknown symbol which appears immediately following
# a single open parenthesis can only be the beginning
# of a definition which makes it an introductor.
# In this case it must be repackaged as an incomplete node.
	
	if syntype == 5:      # A new symbol appears
#		print "New symbol:", item[1] 
		if len(tree) < 2:
			return 0
		if tree[0][0][0] != -2 or tree[1][0][0] != -9:
			return 0
		if len(tree) == 2: 
			if len(tree[1]) ==1:
				item = [[-3,[]], item] # Change new node to incomplete
				header = item[0]
				syntype = header[0]
#				print "New Introductor"
			else:
				return 0
		elif len(tree) == 3:
			if  tree[2][0][0]  in [-1, -3]: 
				pass  # Neither an introductor nor a connector. 
				# if tree[2][0][0] == -1: 
				# tree[2][0][0] = -3 will be done in nodecheck() 
			else:
				return 0
		elif len(tree) == 4 and len(tree[1]) == 1:
			# This must be a new connector.
			if tree[2][0][0] == -2 and  tree[3][0][0] == -12:
				pass
			elif tree[2][0][0] == -2 and  tree[3][0][0] == -9:
#				print "new connector"
				tree[3][0][0] = -12	
				item[0][0] = 3
				item.append(-2)
			else:
				return 0
		else:
			return 0

	if syntype == 6: # Recognize the end of a parade or a definition
		if len(tree) == 0:
			print "Error: Extra right paren"
			return 0
		if tree[-1][0][0]  ==-9: # Parade
			if not paradecrop(tree[-1], 0): #Calls paradecheck
				if len(tree) == 4 and len(tree[3])== 2 and tree[3][1][0][0] == 50:
					parsed_parade = tree.pop().pop()
					current = tree.pop()
					current.append(parsed_parade)
					tree.append(current)
				else:
					print "Error: parade syntax."
					return 0
			else:
				parsed_parade = tree.pop().pop()
				current = tree.pop()
				current.append(parsed_parade)
				tree.append(current)
		elif tree[-1][0][0] == -12: #Undefined parade 
# 			print "Undefined parade", tree[-1]
			current = tree.pop()
			promote(current, 50)
			tree[-1].append(current)
		elif tree[-1][0][0] in [-11, -10]: # New or old definition
			print "Should not reach this point"
			raise SystemExit
	
	if syntype in [1,2]: 
		if len(tree) < 2:
			return 0
		if len(tree) == 3:
			if tree[0][0][0]  == -2 and tree[1][0][0] == -9 and tree[2][0][0] == -3:
				# This is the only valid way out of a -3 state.
				# Change from a parade to a definition in the next if block 
				current = tree.pop()
				promote(current, 49)
				tree[-1].append(current)

	if syntype in [1,2]: # Change parades to definitions
		if tree[-1][0][0] == -9 and len(tree[-1]) == 2:
			if tree[-1][1][0][0] == 49:
				tree[-1][0][0] = -11

	if len(tree) > 1 and len(tree[-1]) > 1 and tree[-1][0][0] == -8: #End scope
#
# No need for recursion here since a scope 
# is not allowed at the end of a formula
#
		lasthead = tree[-2][0]  #Header of the notarian expression
		if syntype == 22:       # Semi-colon
			if scopecheck(tree[-1]):
				if len(lasthead) == 2:# No length determination made yet
					lasthead.append(5)  # E xiA ; px qx
				elif lasthead[2] != 7:
					return 0
				current = tree.pop()
				promote(current, 48)
				tree[-1].append(current)   
			else:
				return 0
		elif syntype == 21:     # Colon
			if scopecheck(tree[-1]):
				if len(lasthead) > 2:
					return 0
				else:
					lasthead.append(4)  # E xiA : px
				current = tree.pop()
				promote(current, 48)
				tree[-1].append(current)   
			else:
				return 0
		elif syntype == 19:   # Left Scope Bracket
			if len(tree[-1]) == 2 and tree[-1][-1][0][0] in [10,14,40,42]:
				lasthead.append(7)  # E ux < xiA ; px >
				current = tree.pop()
				current = current.pop()  # Not a scope after all.
				tree[-1].append(current)   
			else:
				return 0
		elif syntype == 20:     #Right Scope Bracket
			if len(lasthead) <= 2 or lasthead[2] != 7:
				return 0
			elif scopecheck(tree[-1]):
				if len(tree[-2]) == 4 and tree[-2][-1][0][0]== 19:
					lasthead[2] = 6  # E ux < xiA >  , No px after all.
				else:
					return 0
				current = tree.pop()
				promote(current, 48)
				tree[-1].append(current)   
			else:
				return 0
		elif not (tree[-1][-1][0][0] in [3] or syntype in [3]):
			if len(lasthead) != 2:
				pass
			elif scopecheck(tree[-1]):
				okval = 2
				lasthead.append(3)  # E xiA qx
				current = tree.pop()
				promote(current, 48)
				tree[-1].append(current)   
			else:
				return 0
		

################################################################
#
#    Main Algorithm 
#
################################################################
			
	if syntype < 0: # Incomplete nodes are just appended, not checked!
#		print "item = ", item
		tree.append(item)
		if item[1][0][0] == 16 and len(item) == 2:
			tree.append([[-9,0]])
		if item[-1][0][0] in [19, 8, 9]:
			tree.append([[-8,[]]])
		for x in tree:
			if x[0][0] > 0:
				return 0
		return okval
	elif tree:  # When a complete node arrives it is added to the last incomplete
		# node, possibly completing it.  Recursion follows. 
		for i in range(len(tree)):
# Since known introductors can introduce unknown forms
# it catches errors sooner to check for unknown forms here.
			if tree[i][0][0] == -3:
				if i != 2:
					return 0
				if tree[0][0][0] != -2:
					return 0
		current = tree.pop()
		current.append(item)
		ndc = nodecheck(current)
		if ndc == 0:
			return 0
		adn = addnode(tree,current)
		if adn == 0: 
			return 0
		return max(adn,ndc)
	else:       # If no incomplete nodes are left we are done.
		tree.append(item)
		return okval

def promote(node,newvalue): # Change incomplete node to complete
	for k in range(1, len(node)):
		if node[k][0][0] < 40:
			node[k] = node[k][1]
	node[0][0] = newvalue

def nodecheck(item):
# 
#item is a tree with a newly added node
# nodecheck determines whether it should
# converted to a complete node and does
# so if item is ready.  It returns 1 unless
# there is a parse error.
#
#	pprint(item)
	header = item[0]
	syntype = header[0]
	thisheader = item[-1][0]
	if syntype  == -1:
		r = deflistupdate(item)
		if r == 0 :
			header[0] = -3 # Undefined expression
			return 1
		if r == 1:
			d = item[0][1][0] 
			if len(d) == len(item) : 
				if d[0][0]  == 40:
					promote(item, 40)
				elif d[0][0]  == 41:
					promote(item, 41)
				else:
					raise SystemExit
#				item[0].remove(item[0][1])
				defs = mathdb[MD_DEFS]
				item[0][1][0] = defs[item[1]].index(item[0][1][0])
				for x in d[0][1][1:]:
					item[0][1].append(x)
#				item[0][1] = [defs[item[1]].index(item[0][1][0])]
		return 1
	elif syntype == -2: #Parenthetical expression
		if item[-1][0][0] == 45:
			return 1
		elif item[-1][0][0] == 44:
			return 1
		elif item[-1][0][0] == 47:
			return 1
		elif item[-1][0][0] != 6:
			return 0
#		Checks already done.
		if item[-2][0][0] == 44:
			promote(item, 40)
		elif item[-2][0][0] == 45:
			promote(item, 41)
		elif item[-2][0][0] == 47:
			promote(item, 51)
		elif item[-2][0][0] == 50:
			promote(item, 49)
		else:
# This location is reached if x_ instead of x\_ is used"
			return 0
		return 1
	elif syntype == -3:  # Undefined Expression
		if not thisheader[0] in [1,2,6,14,15]:  
			return 1
	elif syntype in [ -4, -5]:  # Schematic Expressions 
		if thisheader[0] in [10, 14, 40, 42] : # A term 
			if len(item) == header[1] + 2: 
				promote(item, 38 - syntype) # Get values: 42 term, 43 formula 
			return 1
	elif syntype == -11:  # A New Definition
		lastheader = item[-2][0]
		if item[1][0][0] != 49:
			print "Mistake"
			raise SystemExit
		if len(item) > 4:
			return 0
		elif len(item) == 3:
			return thisheader[0] in [1,2] 
		elif len(item) == 4:
			if definitioncheck(item):
				promote(item,47)
				return 1
			else:
				print "Error: Failed definition check"
				return 0
	elif syntype == -12:  # Undefined Parade
		if thisheader[0] in [10,11,14,15,40,41,42,43]:
			if len(item) < 3:
				return 1
			if item[-2][0][0] in [10,11,14,15,40,41,42,43]:
				print "Error: Connector missing."
				return 0
			else:
				return 1
		elif thisheader[0] in [3,5,7]:
			return 1 
	elif syntype == -9:  # A Parade
		if thisheader[0] == 49:
			if len(item) == 2:
				item[0][0] = -11
				return 1
		elif thisheader[0] in [10,11,14,15,40,41,42,43]:
			if len(item) < 3:
				return 1
			if item[-2][0][0] in [10,11,14,15,40,41,42,43]:
				header[1] = 1000 # Precedence of the empty connector
				print "Error: Connector missing."
			return 1
		elif thisheader[0] in [5,7,44,45]:
			return 1 
		elif thisheader[0] in [1,2,3]:
			if len(item) == 3 and thisheader[0]in[1,2]and item[1][0][0] == 49:
				item[0][0] = -11
				return 1	
			if item[-1][2] >= header[1]: # compare with parade's max
				header[1] = item[-1][2] 
				return 1
			savelast = item.pop()
			if not paradecrop(item, savelast[2]):
				return 0
			item.append(savelast)
			item[0][1] = savelast[2]
			return 1 
	elif syntype == -8:  # A scope 
		if thisheader[0] in [3,5,7,10,11,14,15,40,41,42,43]:
			return 1
	elif syntype in [-6,-7]:  # A notarian expression 
		lasthead = header[2] 
	 	# x = raw_input("Definition check")  #Find the dft dff access values
		term_or_formula = header[1][0][0]
		if len(item) < 4:      # Scope rescue section does the checking
			return 1
		elif len(item) == 4:
			if lasthead == 7:
				return thisheader[0] == 19  # Left scope bracket
			elif lasthead == 6:
				return thisheader[0] == 19  # Left scope bracket
			elif lasthead == 5:
				return thisheader[0] == 22  # Semi-colon
			elif lasthead == 4:
				return thisheader[0] == 21  # Colon
			elif lasthead == 3:
				if syntype == -6 and thisheader[0] in[10,14,40,42]:
					pass
				elif syntype == -7 and thisheader[0] in[11,15,41,43]:
					pass
				else:
					return 0
				item[0].remove(item[0][1]) # Remove definition list
				promote(item,term_or_formula) 
				item[0][0] = item[0][0][0]
#				print "item inside nc after len 4 = ", item
				return 1
		elif len(item) == 5:
			if lasthead == 7:
				return thisheader[0] == 48
			elif lasthead == 6:
				return thisheader[0] == 48
			elif lasthead == 5:
				if thisheader[0] in [11,15,41,43]:
					return 3
				else:
					return 0
			elif lasthead == 4:
				if thisheader[0] in [11,15,41,43]:
					item[0].remove(item[0][1]) # Remove definition list
					promote(item,term_or_formula) 
#					item[0][:1] = item[0][0]  # Testing??
					item[0][0] = item[0][0][0]
					return 1
		elif len(item) == 6:
			if lasthead == 7:
				return thisheader[0] == 22  # Semi-colon
			elif lasthead == 6:
				if thisheader[0] == 20:     # Right scope bracket
					item[0].remove(item[0][1]) # Remove definition list
					promote(item, term_or_formula)
#					item[0][:1] = item[0][0]  # Testing??
					item[0][0] = item[0][0][0]
					return 1
			elif lasthead == 5:
				if syntype == -6 and thisheader[0] in[10,14,40,42]:
					pass
				elif syntype == -7 and thisheader[0] in[11,15,41,43]:
					pass
				else:
					return 0
				item[0].remove(item[0][1]) # Remove definition list
				promote(item,term_or_formula) 
#				item[0][:1] = item[0][0]  # Testing??
				item[0][0] = item[0][0][0]
				return 1
		elif len(item) == 7:
			if thisheader[0] in [11,15,41,43]:
				return 1
		elif len(item) == 8:
			if thisheader[0] == 20:     # Right scope bracket
				item[0].remove(item[0][1]) # Remove definition list
				promote(item, term_or_formula)
#				item[0][:1] = item[0][0]  # Testing??
				item[0][0] = item[0][0][0]
				return 1
	return 0

def deflistupdate(item):
	deflist = item[0][1]
#	print "Number of defs: ", len(deflist)
	n = len(item) - 1
	a = []
	for definiendum in deflist:
#		definiendum = d[1]
		ibvlist = definiendum[0][1][1:]
		if n in ibvlist:
			if item[-1][0][0] == 10:
				a.append(definiendum)
		elif syntmatch(definiendum[n], item[-1]):
			a.append(definiendum)
	item[0][1] = a
	return len(a)

def definitioncheck(item):
	definiendum = item[1]
	definor = item[2]
	definiens = item[3]
#	print definiendum, definor, definiens
	if definor[0][0] == 1:
		if definiens[0][0] not in [10, 14, 40,42]:  # A Term
			print "Error: Type mismatch, term expected"
			return 0	
	elif definor[0][0] == 2:
		if definiens[0][0] not in [11, 15, 41,43]:  # A formula 
			print "Error: Type mismatch, formula expected"
			return 0	
	return register_definiendum(definiendum,definor[0][0])


def paradecrop(item, reducedmax):
	header = item[0]
	while reducedmax < header[1]:#compare with parade's max
		thisprec = header[1]
		newitem = []
		x = item.pop()
		if x[0][0] in [1,2,3]:
			lastprec = x[2]
		while x[0][0] not in [1,2,3] or x[2] >= thisprec:
			newitem.append(x)
			if len(item) == 1:
				newitem.reverse()
				newitem = [[44, thisprec]] + newitem
				a = paradecheck(newitem)
				if a == 0:
					promote(newitem, 50)
					item.append(newitem)
 					print "Error: Parade syntax."
					return 0
				promote(newitem, a)
				item.append(newitem)
				return 1
			x = item.pop()
			if x[0][0] in [1,2,3]:
				lastprec = x[2]
		if x[0][0] in [1,2,3]:
			item.append(x)
		item[0][1] = lastprec 
		newitem.reverse()
		newitem = [[44, thisprec]] + newitem
		a = paradecheck(newitem)
		if a == 0:
			newitem[0][0] = 50
			item.append(newitem)
			print "Error: Parade syntax"
			return 0
		promote(newitem, a)
		if len(item) > 1:
			item.append(newitem)
		else:
			item.append(newitem)
			break
	return 1


programmed_precedences = [1,2,3,4,5,6,7,9,11,13,15,17,19,25,1000]

def paradecheck(item):
	if len(item)  == 1: 
		print "Error: Empty parade"
		return 0
	prec = item[0][1]
	lastsix = 'Irrelevant initial value'

	for x in item[1:]: #Check for variations in the precedence 
		if x[0][0] in  [1, 2, 3]:
			if x[2] == 6:
				lastsix = x[1]
			if x[2] != prec:
				print "Error: Mixed precedence values:", prec, x[2]
				return 0
	allands = 1
	
	same_op = ''
	allands = 1
	for x in item[1:]:
		if x[0][0] == 3:
			if x[1] != same_op:
				if same_op:
					allands = 0
				else:
					same_op = x[1]
	if allands and same_op in mathdb[MD_CAOPS]: 
   #	Flag commutative-associative expressions for special treatment
		item[0].append(-1)
	if prec < 0:
		return 1
	elif prec == 1:
		if n_arycheck(item):
			return 44
	elif prec == 2:
		if n_arycheck(item,1):
			return 45
	elif prec == 3:
		if condelscheck(item,2):
			return 44
	elif prec == 4:
		if n_arycheck(item,1):
			return 45
	elif prec == 5:
		if n_arycheck(item,1):
			return 45
	elif prec == 6:
		if lastsix == ',':
			if n_arycheck(item):
				return 44
		else: 
			if binverbcheck(item):
				return 45 
	elif prec in [9,13]:
		if mixcheck(item):
			return 44
	elif prec in [7,11,15,17,19,25]:
		if n_arycheck(item):
			return 44
	elif prec == 1000:
		print "Error: Adjacent writing undefined"
		return 0
	else:
		return deflistcheck(item,prec)
	return 0

def binverbcheck(item):
#
# values of state variable:
#    0  subject
#    1  verb
#    2  object
# 
 
	state = 0
	j = 0
  
	for x in item[1:]:
		j = j + 1
		y = x[0][0]
		if state == 0:
			if y == 3 or y == 1:
				if j == 1:
					print "Error: Start with a term."
					return 0
				elif x[1] != ',':
					if x[2] != 6:
						raise "Non verb!"
					state = 1
				elif j % 2 == 1:
					print "Error: Term needed."
					return 0
#				else x[1] == ',' and j % 2 == 0 which is ok.
			elif y in [10,14,40,42,44]:
				if j % 2 == 0: 
					print "Rescue code should catch this"
					raise SystemExit
#				else: This is the indicial variable case
			else:
				print "Error: Term needed."
				return 0
		elif state == 1:
			if y in [10,14,40,42,44]:
				state = 2
			elif y != 3:
				print "Error: Bad Nexus."
				return 0
		elif state == 2:
			if y == 3 or y == 1:
				if x[1] == ',':
					state = 0
					j = 0
			elif y not in [10,14,40,42,44]:
				print "Error: object needed"
				return 0
		else:
			raise "Programming error"
	if state == 2:
		return 1
	else:
		return  0

def scopecheck(item):
	if len(item)  == 1: 
		print "Error: Empty scope."
		return 0
#
# values of state variable:
#    0  subject
#    1  verb
#    2  object
#
	state = 0
	j = 0
# 
	for x in item[1:]:
		j = j + 1
		y = x[0][0]
		if state == 0:
			if y == 3:
				if j == 1:
					print "Error: Indicial variable needed."
					return 0
				elif x[1] != ',':
					if x[2] < 6:
						print "Error: " + x[1] + " not allowed in scope"
						return 0
					state = 1
				elif j % 2 == 1:
					print "Error: Indicial variable or verb needed."
					return 0
#				else x[1] == ',' and j % 2 == 0 which is ok.
			elif y == 10:
				if j % 2 == 0: 
					raise "Rescue code should catch this"
#				else: This is the indicial variable case
			else:
				print "Error: Indicial variable needed."
				return 0
		elif state == 1:
			if y in [10,14,40,42,44]:
				state = 2
			elif y != 3:
				print "Error: Bad nexus."
				return 0
		elif state == 2:
			if y == 3:
				if x[1] == ',':
					state = 0
					j = 0
			elif y not in [10,14,40,42,44]:
				print "Error: object needed"
				return 0
		else:
			raise "Programming error"
	return 1 
	

def n_arycheck(item,tf_flag = 0):
	n = len(item)
	if n < 4:
		return 0
	if item[2][0][0] not in [1,2,3]:
		print "Error: ",item[2][1] , "not a connector"
		return 0
	binarian = item[2][1]
	for i in range(1,n):
		if i % 2 == 1:
			if tf_flag == 0:
				if item[i][0][0] not in [10,14,40,42,44]:
					print "Error: ", item[i], "is not a term"
					return 0
			else:
				if item[i][0][0] not in [11,15,41,43,45]:
					print "Error: ", item[i], "is not a formula"
					return 0
		else:
			if item[i] != item[2]: 
				print "Error: ", item[i], "!=", item[2]
				return 0
	return 1

def mixcheck(item):
	n = len(item)
	if n < 3:
		return 0
	if item[1][0][0] == 3:
		if item[2][0][0] not in  [10,14,40,42,44]:
			return 0
	for i in range(1,n -2):
		if item[i][0][0] in [10,14,40,42,44]:
			if item[i+1][0][0] != 3:
				return 0
			if item[i+2][0][0] not in [10,14,40,42,44]:
				return 0
		elif item[i][0][0] != 3:
			return 0
	return 1

def binarycheck(item,tf_flag = 0):
	n = len(item)
	if n != 4:
		return 0
	if item[2][0][0] != 3:
		return 0
	if tf_flag == 0:# both terms
		if item[1][0][0] not in [10,14,40,42,44]:
			return 0
		if item[3][0][0] not in [10,14,40,42,44]:
			return 0
	elif tf_flag == 1:# both formulas
		if item[1][0][0] not in [11,15,41,43,45]:
			return 0
		if item[3][0][0] not in [11,15,41,43,45]:
			return 0
	elif tf_flag == 2:# formula term
		if item[1][0][0] not in [11,15,41,43,45]:
			return 0
		if item[3][0][0] not in [10,14,40,42,44]:
			return 0
	return 1

def condelscheck(item,tf_flag = 0):
	n = len(item)
	if n < 4 or n % 2 == 1:
		return 0
	for k in range(1,n,2): # All terms or formulas
		if item[k][0][0] in [10,14,40,42,44]:
			pass
		elif item[k][0][0] in [11,15,41,43,45]:
			pass
		else:
			return 0
	return 1
	for k in range(2,n,2):
		if item[k][1] != '\\els':
			break
	else:
		for k in range(1,n,2): # All terms
			if item[k][0][0] not in [10,14,40,42,44]:
				return 0
		return 1
	for k in range(2,n,2):
		if (k/2)%2 == 1:
			if item[k][1] != '\\cond':
				return 0
			if item[k-1][0][0] not in [11,15,41,43,45]:
				return 0
		else:
			if item[k][1] != '\\els':
				return 0
			if item[k-1][0][0] not in [10,14,40,42,44]:
				return 0
	if item[n-1][0][0] not in  [10,14,40,42,44]:
		return 0
	return 1

def deflistcheck(item,prec):
#For parenthetical expressions only.
	defs = mathdb[MD_DEFS]
	for k in range(len(defs[prec])):
		if len(item) != len(defs[prec][k]):
			continue
		for j in range(1,len(item)):
			if not syntmatch(defs[prec][k][j], item[j]):
				break
		else:
			return item[0][0] # Parade Term or Formula Indicator
	return 0

def register_definiendum(definiendum,termorformula):
	td = mathdb[MD_SYMTYPE]
	precedence = mathdb[MD_PRECED]
	defs = mathdb[MD_DEFS]
	introductor = definiendum[1] 
#	print "DEFINIENDUM =", definiendum

	if symtype(definiendum[1]) == 16: # Left parenthesis
		if definiendum[2][0][0] != 50:
			return 0
		if definiendum[3]!= ')':
			return 0
		definiendum = definiendum[2]
		if termorformula == 1:
			definiendum[0][0] = 44
		else:
			definiendum[0][0] = 45

		precedence_from_context = -1
		for x in definiendum[1:]:
			if type(x) is str :
				if symtype(x) in [1,2,3]:
					if precedence_from_context == -1:
						precedence_from_context = precedence[x]
					elif precedence_from_context != precedence[x]:
						print "Error: Mixed precedence values in definiendum!"
						return 0

		for x in definiendum[1:]:
			if type(x) is str:
				if symtype(x) == 5 or symtype(x) == 7:
#					print "New connector", x
					if precedence_from_context > -1:
						precedence[x] = precedence_from_context
					elif  x in precedence:
						pass
					else:
						print "Error: Precedence not set for ", x
						return 0
					td[x] = 3
					p = precedence[x]
				elif symtype(x) in [1,2,3]:
					p = precedence[x]
		if p not in programmed_precedences:
#			definiendum[0][1] = p
			if p in defs.keys():
				definiendum[0][1] = [len(defs[p])-1]
				defs[p].append(definiendum)
			else:
				definiendum[0][1] = [0]
				defs[p]= [definiendum]
		elif len(definiendum) == 4:
			pass
		else:
			print "Error: Non-binary connector with programmed precedence!"
			return 0
	else:  # An introductor which is not a parenthesis
		if termorformula == 1: # A term
			definiendum[0][0] = 40
		else:                  # A formula 
			definiendum[0][0] = 41
		bvlist = []
		varlist = []
		schematorlist = []
		term_schemexp = []
		formula_schemexp = []
		for x in definiendum[2:]:
			if type(x) is str: 
				if symtype(x) == 5:
						td[x] = 7
				elif symtype(x) in [10,11]:
					if x in varlist:
						print "Error: Repeated occurrence of ", x
						return 0
					else:
						varlist.append(x)
			elif x[0][0] in [42,43]:
				if x[1] in schematorlist:
					print "Error: ",x[1], "repeated schemator in definiendum"
					return 0
				else:
					schematorlist.append(x[1])
				if x[0][0] == 42:
					term_schemexp = x
				else:
					formula_schemexp = x
				for y in x[2:]:
					if type(y) is not str or symtype(y) != 10:
						print "Error: Non-variable ",y, "not allowed"
						return 0
					elif y not in bvlist:
						bvlist.append(y)
				#print "Bound variables", bvlist 
			else:
				y = norepeat_varlist(x)
				if y == 0: 
					print "Error: Repeated variable"
					return 0
				else:
					for z in y:
						if z in varlist:
							print "Error: Repeated occurrence of ", z
							return 0
						elif symtype(z) in [12,13]:
							print "Error: Imbedded schemator", y 
							return 0
						else:
							varlist.append(z)
		for y in bvlist:
			if y not in varlist:
				print "Error: Indicial position missing for", y
				return 0
		if bvlist:
			initialsegment =   [introductor, bvlist[0]]
			for y in bvlist[1:]:
				initialsegment = initialsegment + [',', y]
		if term_schemexp and formula_schemexp and\
            term_schemexp[2:] == formula_schemexp[2:] == bvlist and\
				definiendum[1:] == initialsegment + [';', formula_schemexp, term_schemexp]:
			if symtype(introductor) == 5:
				td[introductor] = 8
				defs[introductor] = [definiendum]  # Needed to determine T or F status
#				print "Error: Term Seeking Notarian" , introductor, definiendum
			else:
				print "Error: Defining a known constant as a notarian not allowed." 
				return 0
		elif formula_schemexp and formula_schemexp[2:] == bvlist and\
            definiendum[1:] == [introductor] + bvlist + [formula_schemexp]:			
			if symtype(introductor) == 5:
				td[introductor] = 9
				defs[introductor] = [definiendum]  # Needed to determine T or F status
#				print "Error: Formula Seeking Notarian", introductor, definiendum 
			else:
				print "Error: Defining a known constant as a notarian not allowed." 
				return 0
		else:
			ibvlist = []
			for k in range(len(definiendum)):
				if definiendum[k] in bvlist:
					ibvlist.append(k)
			if symtype(introductor) == 5:
				if len(definiendum) == 2:
					if termorformula == 1: 
						td[introductor] = 14
					else:
						td[introductor] = 15
				else:
					definiendum[0][1:] = [[0]]
					definiendum[0][1].extend(ibvlist)
					defs[introductor] = [definiendum]
					td[introductor] = 17
			elif symtype(introductor) == 17:
#           If definiendum is subsumed it just parses.
				definiendum[0][1:] = [[len(defs[introductor])]]
				definiendum[0][1].extend(ibvlist)
				defs[introductor].append(definiendum)
#				introductor of parsed_exp = parsed_exp[1]  
#				definiendum of parsed_exp = defs[introductor][parsed_exp[0][1][0]]
#				definiendum = defs[introductor]
#				ibvlist of definiendum = definiendum[0][1][1:]
	return 1
	

def syntmatch(form, instance):
	headeri = instance[0]
	arity = mathdb[MD_ARITY]
	if type(form) is str :
		if symtype(form) == 10: 
# This should never happen.
			return headeri[0] in [10,14,40,42] 
		elif symtype(form) == 11:
			return headeri[0] in [11,15,41,43] 
		else:
			return form == instance[1]
	else:
		headerf = form[0]
		if headerf[0] == 42:
# This should never happen.
			return headeri[0] in [10,14,40,42] 
		if headerf[0] == 43:
# This should never happen.
			return headeri[0] in [11,15,41,43] 
		if headerf[0] in [40,41]:
			indvars = indvlist(form)
			for n in range(1,len(form)):
				ok = subsyntmatch(form[n],instance[n],indvars)
				if not ok:
					return 0
			return 1

def subsyntmatch(form, instance,indvars):
	headeri = instance[0]
	arity = mathdb[MD_ARITY]
	if type(form) is str: 
		if symtype(form) == 10: 
			if form in indvars:
				return headeri[0] == 48 
			elif type(instance) is str: 
				return symtype(instance) in [10,14]
			else:
				return headeri[0] in [10,14,40,42] 
		elif symtype(form) in [11,12,13] and arity[form] == 0: # form is a schemator
			return headeri[0] in [11,15,41,43] 
		else:
			return form == instance
	else:
		headerf = form[0]
		if headerf[0] == 42:
			return headeri[0] in [10,14,40,42] 
		if headerf[0] == 43:
			return headeri[0] in [11,15,41,43] 
		if headerf[0] in [40,41]:
			for n in range(1,len(form)):
				ok = subsyntmatch(form[n],instance[n],[])
				if not ok:
					return 0
			return 1

def intext(mode,linetail,outfragments = None):
	TeXdollars = pattern.TeXdollar.search(linetail[0])
	Noparsem = pattern.Noparse.search(linetail[0])
	if TeXdollars:
		if Noparsem:
			if Noparsem.start(1) < TeXdollars.start(1): 
				if type(outfragments) is list: 
					outfragments.append(linetail[0])
				linetail[0] = ''
		else:
			mode[0]= 2  # Change to math-on mode
			if type(outfragments) is list:
				outfragments.append(linetail[0][:TeXdollars.end(1)])
			linetail[0] = linetail[0][TeXdollars.end(1):]
	elif Noparsem:
		if Noparsem.group('TeXcomment'): 
			p = process_directive(linetail[0])
			if p == -1:
#				print "New primitive formula" 
				pass
			elif p == -2:
#				print "New primitive term" 
				pass
			elif p == -7 : 
				mode[0] = 4
				return
		if type(outfragments) is list :
			outfragments.append(linetail[0])
		linetail[0] = ''
	else:
		if type(outfragments) is list :
			outfragments.append(linetail[0])
		linetail[0] = ''
	return

def process_directive(comment_line, hereditary_only=True):
	directivem = pattern.directive.match(comment_line)
	if not directivem:
		return 0
	if directivem.group(1) == 'set_precedence':
		if not directivem.group(4).isdigit():
			print "Error: Numerical precedence value needed."
			return -7
		if directivem.group(3) in  mathdb[MD_PRECED]:
			if mathdb[MD_PRECED][directivem.group(3)] == int(directivem.group(4)):
				return 0
			else:
				print "Error: Precedence already defined as ",mathdb[MD_PRECED][directivem.group(3)] 
		mathdb[MD_PRECED][directivem.group(3)] = int(directivem.group(4))
		if mathdb[MD_SYMTYPE].get(directivem.group(3))in [1,2]:
			pass
		else:
			mathdb[MD_SYMTYPE][directivem.group(3)] = 3
		return -3
	elif directivem.group(1) == 'def_symbol':
		mathdb[MD_MACR][directivem.group(3)[1:]] = directivem.group(4)[1:].rstrip()
	elif directivem.group(1) == 'external_ref' and not hereditary_only:
		mathdb[MD_REFD][directivem.group(3)] = directivem.group(4).rstrip()
	elif directivem.group(1) == 'major_unit:':
		# Handle this in renum.
		pass
	elif directivem.group(1) == 'subfile:':
		# Handle this in makedf.
		pass
	elif directivem.group(1) == 'term_definor:':
		mathdb[MD_SYMTYPE][directivem.group(2).strip()] = 1 
	elif directivem.group(1) == 'formula_definor:':
		mathdb[MD_SYMTYPE][directivem.group(2).strip()] = 2 
	elif directivem.group(1) == 'rules_file:':
		mathdb[MD_RFILE] = directivem.group(2).strip()
	elif directivem.group(1) == 'props_file:':
		mathdb[MD_PFILE] = directivem.group(2).strip()
	elif directivem.group(1) == 'undefined_term:':
		tree = []
		tempmode = [2]
		linetailcopy = [directivem.group(2).strip()]
		addtoken(tree,'(')
		mathparse(tempmode, linetailcopy, tree)
		addtoken(tree,'\ident')
		if tree[-1][0][0] == -11:
			new_term = tree[-1][1]
#			print  "Primitive term: ", directivem.group(2) 
			rd = register_definiendum(new_term, 1)
			if not rd:
				print "Error: Register definiendum failed on ", new_term
				return -7
			return -2	
			promote(tree[-1],45)
		elif tree[-1][1][0][0] == 40: 
			pass
		elif tree[-1][1][0][0] == 14: 
			pass
		else:
			print "Error: Could not parse primitive term"
			return -7
	elif directivem.group(1) == 'undefined_formula:':
		tree = []
		tempmode = [2]
		linetailcopy = [directivem.group(2).strip()]
		addtoken(tree,'(')
		mathparse(tempmode, linetailcopy, tree)
		addtoken(tree,'\Iff')
		if tree[-1][0][0] == -11:
			new_formula = tree[-1][1]
#			print  "Primitive formula: ", directivem.group(2) 
			rd = register_definiendum(new_formula, 2)
			if not rd:
				print "Error: Register definiendum failed on", new_formula
				return -7
			return -1	
			promote(tree[-1],45)
		elif tree[-1][1][0][0] == 41: 
			pass
		elif tree[-1][1][0][0] == 15: 
			pass
		else:
			print "Error: Could not parse primitive formula"
			return -7
	return 0

def mathmargin(mode,linetail,outfragments = None):	
	newlinetail = linetail[0].lstrip()
	trimlen = len(linetail[0]) - len(newlinetail)
	if trimlen:
		blanks = linetail[0][:trimlen]
		if type(outfragments) is list: 
			outfragments.append(blanks)
		linetail[0] = newlinetail
	if linetail[0]:
		TeXdollarm = pattern.TeXdollar.match(linetail[0])	
		if pattern.TeXcomment.match(linetail[0]):	
			if type(outfragments) is list: 
				outfragments.append(linetail[0])
			linetail[0] = ''
		elif TeXdollarm: 
			if type(outfragments) is list:
#				outfragments.append(linetail[0][:1])
				outfragments.append(linetail[0][:TeXdollarm.end(1)])
			linetail[0] = linetail[0][TeXdollarm.end(1):]
			mode[0] = 2
		else:
			notem = pattern.note.match(linetail[0])
			if notem:
				print "Error: Previous note unfinished."
				mode[0] = 4
				return
			else:
				linem = pattern.line.match(linetail[0])
				if linem:
					nn = linem.start(2) - 1
					if type(outfragments) is list: 
						outfragments.append(linetail[0][:nn])
					linetail[0]=linetail[0][nn:]
				elif linetail[0][:3] ==  '\\By':
					if type(outfragments) is list: 
						outfragments.append(linetail[0])
					linetail[0] = ''
				else:
					mode[0] = 4  # Signal an error
	return

def notemargin(mode,linetail):	
	if not linetail[0]:
		return
	TeXcommentm = pattern.TeXcomment.match(linetail[0])
	TeXdollarm = pattern.TeXdollar.match(linetail[0])
	if TeXcommentm: 
		linetail[0] = ''
		return
	elif TeXdollarm: 
		linetail[0] = linetail[0][TeXdollarm.end(1):]
		mode[0] = 2
		return
	linem = pattern.line.match(linetail[0])
	if linem:
		linetail[0]=linem.group(2)
		mode[0] = 2
		return
	bym = pattern.by.match(linetail[0])
	if bym:
		rp = refparse(bym.group(1))
		if rp == 0:
			print "Error in reference: ",bym.group(1)
			mode[0] = 4
		else:
			linetail[0] = ''
		return
	newlinetail = linetail[0].lstrip()
	TeXdollarm = pattern.TeXdollar.match(newlinetail)
	TeXcommentm = pattern.TeXcomment.match(newlinetail)
	if not newlinetail: 
		linetail[0] = ''
	elif TeXcommentm: 
		linetail[0] = ''
	elif TeXdollarm: 
		linetail[0] = newlinetail[TeXdollarm.end(1):]
		mode[0] = 2
	else:
		print "Error: Only references allowed in note margins"
		mode[0] = 4  # Signal an error
	return

def stringparse(wffstring):
	line_list = [wffstring]
	linetail = [line_list[0],0,1,line_list]
	parsetree = []
	mode = [2]
	mathparse(mode,linetail,parsetree)
	return parsetree[0]

def mathparse(mode,linetail,tree,outfragments=None,pfcdict=None):
	if len(linetail) == 1:
		currentpos = 0
	else:
		currentpos = linetail[1]
	if mode[0] == 4:
		return
	currentline = linetail[0]
	lenline = len(currentline)
	blanklinem = pattern.blankline.match(currentline,currentpos)
	if blanklinem:
			if tree[0][0][0] > 0:   # If the parse is done
				mode[0] = 1  # Change to text mode
			else:
				mode[0] = 3  # Change to Margin mode
			currentpos = blanklinem.end(2)
			if type(outfragments) is list: 
				outfragments.append(currentline[:currentpos])
			linetail[0] = currentline[currentpos:]
			return
	tokenm = pattern.token.match(currentline,currentpos)
	if not tokenm:
		print "Error: Line empty following TeX dollar sign"
		mode[0] = 4
		return		
	if tokenm.group(1):
		if type(outfragments) is list: 
			outfragments.append(tokenm.group(1))
		currentpos = tokenm.end(1)
	while currentpos < lenline: 
		TeXdollarm = pattern.TeXdollar.match(currentline,currentpos)
		if TeXdollarm:
			if tree[0][0][0] > 0:   # If the parse is done
				mode[0] = 1  # Change to text mode
			else:
				mode[0] = 3  # Change to Margin mode
			if type(outfragments) is list: 
				outfragments.append(currentline[currentpos])
			currentpos = TeXdollarm.end(1)
			linetail[0] = currentline[currentpos:]
			return
		if tree != [] and tree[0][0][0] > 0: # If the parse is done
			mode[0] = 5  # Change to end mode
			linetail[0] = currentline[currentpos:]
			return
		tokenm = pattern.token.match(currentline,currentpos)
		if not tokenm:
			mode[0] = 4
			return
		if tokenm.group(1):
			if type(outfragments) is list: 
				outfragments.append(tokenm.group(1))
		token = tokenm.group(2)
		if pfcdict == None or len(token) == 1:
			pfctoken = token
		elif token[1:] in pfcdict:
			pfctoken = "\\"+ pfcdict[token[1:]]
		else:
			pfctoken = token
		ck = addtoken(tree, pfctoken)
		if tokenm.group(1):
			outfragments.append(tokenm.group(1))
		currentpos = tokenm.end(0) 
		linetail[0] = currentline[currentpos:]
		if not ck:
			mode[0] = 4
			linetail[0] = currentline[currentpos:]
			return 
		if type(outfragments) is list: 
			if ck == 1:
				outfragments.append(token)
			elif ck == 2:
				outfragments.append('\\' + pattern.skipstring)
				outfragments.append(token)
			elif ck == 3:
				outfragments.append(token)
				outfragments.append('\\' + pattern.skipstring)
			else:
				outfragments.append(token)
			if tokenm.group(4):
				outfragments.append(tokenm.group(4))
	return 

	
def refparse(ref):
	reflast = False
	reflist = []
	t = ref
#	if not t.strip():
#		print "Error: \\By without any justification"
#		return 0
	while t:
		t = t.lstrip()
		if not t: break
		if t[0] == '$': return 0
		if t[0] == '.' or t[0].isdigit():
			refmatch = pattern.ref.match(t)
			if not refmatch: return 0
			if reflast: 
				print "Error: Punctuator missing"
				return 0
			reflast = True
			reflist.append(refmatch.group(1)+ refmatch.group(4))
			if refmatch.group(4):
				if refmatch.group(4) not in mathdb[MD_REFD]:
					print refmatch.group(4), "undefined file reference key."
					return 0
			t = refmatch.group(5)
			continue
		reflast = False
		punctsmatch = pattern.puncts.match(t)
		puncts = punctsmatch.group(1)
		findsinglematch = pattern.findsingle.match(puncts)
		if findsinglematch:
			if findsinglematch.start(1) == 0:
				reflist.append(puncts[0])
				t = t[1:]
			elif findsinglematch.group(1) in reference_punctuator_list: 
				reflist.append(findsinglematch.group(1))
				t = t[findsinglematch.start(2):]
			else:
				print "Error:",findsinglematch.group(1) , " not in reference_punctuator_list"
				return 0
		else:
			u = punctsmatch.group(2)
			if len(puncts)>4 and puncts[-5:] == '\\char':
				nummatch = pattern.nums.match(u)
				if nummatch:
					reflist.append(puncts + nummatch.group(1))
					t = nummatch.group(2)
				else:
					print "Error: \\char without number" 
					return 0
			elif puncts in reference_punctuator_list:
				reflist.append(puncts)
				t = u 
			else:
				print "Error:",puncts ,"not in reference_punctuator_list"
				return 0
	return reflist

def ruleparse(textline):
	rule = []
	rulevars = []
	rulesignature = []
	t = textline
	while t:
		t = t.lstrip()
		if not t: break
		if t[0] == '$':
			TeXmatch = pattern.TeXmath.match(t)
			if not TeXmatch:
				print "Error: Unmatched Tex dollar sign"
				return 0
			rvar = workparse(TeXmatch.group(1))
			if rvar == 0:
				print "Error: Bad rule", TeXmatch.group(1), "in", textline
				return 0
			rule.append(rvar)
			rulesignature.append('$')
			t = TeXmatch.group(2)
			for x in varlist(rvar):
				if x not in rulevars:
					rulevars.append(x) 
			continue
		punctsmatch = pattern.puncts.match(t)
		puncts = punctsmatch.group(1)
		findsinglematch = pattern.findsingle.match(puncts)
		if findsinglematch:
			if findsinglematch.start(1) == 0:
				rule.append(puncts[0])
				rulesignature.append(puncts[0])
				t = t[1:]
			elif findsinglematch.group(1) in reference_punctuator_list: 
				rule.append(findsinglematch.group(1))
				rulesignature.append(findsinglematch.group(1))
				t = t[findsinglematch.start(2):]
			else:
				print "Error: ",findsinglematch.group(1) , " not in reference_punctuator_list"
				return 0
		else:
			u = punctsmatch.group(2)
			if len(puncts)>4 and puncts[-5:] == '\\char':
				nummatch = pattern.nums.match(u)
				if nummatch:
					rule.append(puncts + nummatch.group(1))
					rulesignature.append(puncts + nummatch.group(1))
					t = nummatch.group(2)
				else:
					print "Error: \\char without number" 
					return 0
			elif puncts in reference_punctuator_list:
				rule.append(puncts)
				rulesignature.append(puncts)
				t = u 
			else:
				print "Error: ",puncts ,"not in reference_punctuator_list"
				return 0
	if '\\C' in rulesignature:
		turnstyle_spot = rulesignature.index('\\C')
	else:
		print "Error: No turnstyle in rule"
		return 0
	assert len(rulevars) == len(set(rulevars))
	return [rule, rulevars, rulesignature]

def sigsig(parsedrule):
	[rule, rulevars, sig] = parsedrule
	assert len(rule) == len(sig)
	depth = 0
	retlist = []
	for k in range(len(sig)):
		if sig[k] == '(':
			if depth == 0:
				chunk = []
			depth = depth + 1
		elif sig[k] == ')':
			depth = depth - 1
			if depth == 0:
				retlist.append(chunk)
		elif sig[k] == '$':
			localvarlist = []
			if type(rule[k]) is list and len(rule[k][0])< 2:
				assert rule[k][0] == [11]
				localvarlist = [rule[k][1]]
			else:
				for x in nblist(rule[k]):
					if x not in localvarlist: 
						localvarlist.append(x)
			if depth == 0:
				chunk = localvarlist
				retlist.append(chunk)
			else:
				for x in localvarlist:
					if x not in chunk:
						chunk.append(x)
		elif sig[k] in reference_punctuator_list:
			assert '(' not in sig[k] and ')' not in sig[k]
			if depth == 0:
				retlist.append(sig[k])
		else:
			print "Programming error:", sig[k]
	return retlist


def getformula(linetail,verbose=True):
	mode = [2]
	parsetree = []

	fetched_tf = ''
	while linetail[0] and (mode[0]== 2 or mode[0] == 3):
		if mode[0] == 2:
			TeXdollars = pattern.TeXdollar.search(linetail[0])
			if TeXdollars:
				stuff = linetail[0][:TeXdollars.start(1)]
			else:
				stuff = linetail[0].strip() 
			fetched_tf= fetched_tf + ' ' + stuff 
			mathparse(mode,linetail,parsetree)
		elif mode[0] == 3:
			mathmargin(mode,linetail)
		if not linetail[0]:
			getline(linetail,verbose)
			while linetail[0][0] == '%':
				getline(linetail,verbose) 
	if mode[0] == 4:
		return []
	if mode[0] == 5:
		print "Error: At most one term or formula allowed."
		return []
	catch = parsetree[0][0][0]
	if parsetree[0][0][0] in [10,11]:
		return[fetched_tf,linetail[0],parsetree[0][1]]
	return [fetched_tf,linetail[0],parsetree[0]]
 
def getline(linetail,verbose=False):
# linetail = [tail of first line, index into tail, line number, list of all lines] 
	if linetail[2] == len(linetail[3]):
		linetail[0] = ''
		return linetail[0]
	linetail[0] = linetail[3][linetail[2]] 
	linetail[2] = linetail[2] + 1 # line_num
	if verbose and linetail[2] % 100 == 0:
		print linetail[2] / 100,
		sys.stdout.flush()
	return linetail[0] 



def varlist(pexp):
	if type(pexp) is str:
		if symtype(pexp) in [10,11,12,13]:
			return [pexp]
		else:
			return []
	elif type(pexp) is list:
		r = []
		for t in pexp:
			s = varlist(t)
			for u in s:
				if not u in r:
					r.append(u)
		return r
	else:
		return []

def norepeat_varlist(pexp):
	if type(pexp) is str: 
		if symtype(pexp) in [10,11,12,13]:
			return [pexp]
		else:
			return []
	elif type(pexp) is list: 
		r = []
		for t in pexp:
			s = varlist(t)
			if s == 0 : return 0
			for u in s:
				if u in r:
					return 0
				else:
					r.append(u)
		return r
	else:
		return []

def scopecondition(scopenode):
	state = 0
	tree = []
	addtoken(tree,'(')
	verbfound = 0
	for xi in scopenode[1:]:
		if type(xi) is list: 
			if state == 1:
				state = 2
			addnode(tree,xi)
		elif symtype(xi) == 14:
			if state == 1:
				state = 2
			addtoken(tree,xi)
		elif symtype(xi) == 10:
			if state == 0: 
				pass	
			elif state == 1:
				state = 2
			addtoken(tree,xi)
		elif xi ==  ',':
			if state == 2:
				state = 0
				addtoken(tree,'\\And')
			else:
				addtoken(tree,xi)
		elif symtype(xi) == 3:
			if state == 0:
				state = 1
				verbfound = 1
			addtoken(tree,xi)
		else:
			raise "Scope error"
	if verbfound:
		addtoken(tree,')')
		return tree[0][2]
	else:
		return []

def verbexpand(pexp):
	if type(pexp) is str or  pexp[0][1] != 6:
		print pexp,
		raise  " sent to verbexpand"
	verblist = []
	termlist = []
	lastterm = []
	lastverb = []
	subjects = []
	conjunctlist = []
	for x in pexp[1:] + ['=']:
		if x == ',':
			if lastterm == lastverb == []:
				print "Initial commas and double commas not allowed"
				raise SystemExit 
			elif lastterm == []:
				verblist.append(lastverb) 
				lastverb = []
			elif lastverb == []:
				termlist.extend(lastterm)
				lastterm = []
		elif type(x) is list or symtype(x) in[10,14]:
			if lastterm != []:
				raise "Parse failure"
			lastterm = [x]
			if lastverb != []:
				verblist.append(lastverb)
				modifiers = verblist
				lastverb = []
				verblist = []
			elif verblist != []:
				raise "Parse wrong"
		else:
			if lastverb == verblist == []:
				if lastterm != []:
					termlist.extend(lastterm)
					lastterm = []    # Here is the possible change!!!!!!!!!!!
				if len(termlist) == 1:
					object = termlist[0]
				else:
					object = [[44, 6]]
					for y in termlist:
						object.append(y)
						object.append(',')
					del object[-1]
				for s in subjects:
					clause = [[45,6]]
					clause.append(s)
					clause.extend(modifiers[-1])
					clause.append(object)
					conjunctlist.append(clause)
					for t in subjects:
						if t is s: break
						for r in modifiers[:-1]:
							clause = [[45,6]]
							clause.append(t)
							clause.extend(r)
							clause.append(s)
							conjunctlist.append(clause)
				if len(termlist) > 1 and lastterm == []:
					subjects = termlist
				else:
					subjects = [object]
#
#To accomodate Morse's tuple scheme uncomment this:		
#
# 				if len(termlist) == 1 and lastterm == []:
# 					lastverb.append(',')
#
				lastterm = []
				termlist = []
			lastverb.append(x)
	if verblist != [] or lastverb != ['=']:
		print "Mistake"
		raise SystemExit
	if len(conjunctlist)==1:
		return conjunctlist[0]
	else:
		if '\\And' in mathdb[MD_CAOPS]:
			retlist = [[45,5,-1] ,conjunctlist[0]]
		else:
			retlist = [[45,5] ,conjunctlist[0]]
		for x in conjunctlist[1:]:
			retlist.append('\\And')
			retlist.append(x)
		return retlist


	
def workparse(strexp):
	parse_tree = []
	mode = [2]
	mathparse(mode,[strexp],parse_tree)
	if mode[0] != 4:
		return deep(parse_tree[0])
	else:
		return 0

def negdeep(exp):
	newexp = deep(exp)
	if type(newexp) is str: 
		return newexp
	elif len(newexp) == 4 and newexp[2] == '\\c' and newexp[1] == '\\true':
		return negdeep(newexp[3])
	if len(newexp) == 4 and newexp[2] == '\\c' and newexp[3] == '\\false':
		if type(newexp[1]) is list and len(newexp[1])== 3 and newexp[1][1] == '\\Not':
			return newexp[1][2]
		else:
			return [[41, [0]],'\\Not', newexp[1]] 
	else:
		return newexp

def deep(exp):
	cmopp = cmop(exp)
	if type(exp) is str: 
		return exp
	elif exp[1] == '(':
		return deep(exp[2])
	elif exp[0][0] in [14,15]:
		return exp[1]
#	elif len(exp) == 4 and exp[2] == '\\c' and exp[1] == '\\true':
#		return deep(exp[3])
	elif exp[0][0] == 45 and exp[0][1] == 6:
		recurselist = [exp[0]]
		for r in exp[1:]:
			recurselist.append(deep(r))
		return verbexpand(recurselist) 
	elif cmopp:
		retlist = [exp[0]]
		for r in exp[1:]:
			d = deep(r)
			if cmop(d) == cmopp:
				retlist.extend(d[1:])
			else:
				retlist.append(d)
		return retlist
#	elif exp[0] == [45,5,1]:
#		retlist = [exp[0]]
# 		for r in exp[1:]:
#			d = deep(r)
#			if type(d) is str: 
#				retlist.append(d)
#			elif d[0] == [45,5,1]:
#				retlist.extend(d[1:])
#			else:
#				retlist.append(d)
#		return retlist
	elif len(exp[0]) > 1 and\
        exp[0][0] in [40,41] and\
        exp[0][1] in [3,4,5,6,7]:
 		return notariancondense(exp)
	else:
		retlist = [exp[0]]
		for x in exp[1:]:
			retlist.append(deep(x))
		return retlist
	

def newvarlist(list):
	global newvarnum
# This is only called by notariancondense when some variable
# occurs in both indicial and accepted positions
	r = []	
	for x in list:
		newvarnum = newvarnum + 1
		r.append('v_{' + ("%d" % newvarnum) + '}')
	return r

def notariancondense(pexp):
	styp = symtype(pexp[1])
	ntyp = pexp[0][1]
	indvs = indvlist(pexp)
	accvs = accvlist(pexp)
	for x in indvs:
		if x in accvs:
			nlist = newvarlist(indvs)
			newform = indvsubst(nlist, indvs, pexp)
			indvs = nlist
			break
	else:
		newform = subst([],[],pexp)
	newscope = [[48,[]], indvs[0]]
	for x in indvs[1:]:
		newscope.append(',')
		newscope.append(x)
		
	for x in newform[1:]:
		if type(x) is str: 
			pass
		elif x[0][0] == 48:
			scopecond = scopecondition(x)
	if ntyp == 4:
		if scopecond:
			scopecond = makeand(scopecond, deep(newform[-1] ))
		else:
			scopecond = newform[-1]
	elif ntyp in [5,7]:
		if scopecond:
			scopecond = makeand(scopecond, deep(newform[-2])) 
		else:
			scopecond = newform[-2]
	if scopecond == []:
		if styp == 8 or ntyp in [6,7]:
			scopecond = '\\true'

	if ntyp in [3,5]:
		indform = newform[-1]
	elif ntyp == 4 :
		if len(indvs) == 1:
			indform = indvs[0]
		else: #We may never use this:
			indform = [[45,6]] + newscope[1:]
	else:
	 	indform = newform[2] 

	if ntyp in [6,7]and styp == 9:
		if ntyp == 7:
			newform[2] = deep(newform[2])
			newform[4] = newscope 
			newform[6] = deep(scopecond)
		elif ntyp == 6:
			newform[0][1] = 7
			newform[2] = deep(newform[2])
			newform[4] = newscope 
			newform[5:5] = [';',deep(scopecond)]
	elif styp == 8:
		newform[2:] = []
		newform[0][1] = 5
		newform.append(newscope)
		newform.append(';')
		newform.append(deep(scopecond))
		newform.append(deep(indform))
	else:
		newform[2:] = []
		newform[0][1] = 3
		indform = deep(indform)
		if newform[1]== '\\Each' :
			if ntyp == 4: 
				raise "Colon not expected"
			elif type(indform) is list and\
      indform[0] == [45,2] and\
      indform[2] == '\\c' and scopecond:
				indform[1] = makeand(deep(scopecond), indform[1])
			elif scopecond:
				indform = [[45,2],deep(scopecond), '\c', indform]
		elif scopecond:
			indform = makeand(deep(scopecond),indform)
		newform.append(newscope)
		newform.append(indform)

		if len(indvs) > 1 and newform[1] in['\\Each','\\Some']:
			indform = deep(newform[-1])
			singlequant = newform[:2]
			rindvs = indvs[:]
			rindvs.reverse() 
			for x in rindvs:
				newscope = [[48,[]], x]
				newform = singlequant + [newscope] 
				newform.append(indform)
				indform = newform
	return newform 

def makeand(exp1, exp2):
	if '\\And' in mathdb[MD_CAOPS]:
		andback = [[45,5,-1]]
	else:
		andback = [[45,5]]
	if cmop(exp1) == '\\And':
		andback.extend(exp1[1:])
	else:
		andback.append(exp1)
	andback.append('\\And')
	if cmop(exp2) == '\\And':
		andback.extend(exp2[1:])
	else:
		andback.append(exp2)
	return andback

def bvarreplace(form, newbvarlist):
	if type(form) is not list: 
		return form 
	oldindvlist = indvlist(form)
	if oldindvlist:
		newindvlist = []
		for x in oldindvlist:
			newindvlist.append(newbvarlist.pop())
		nextform = indvsubst(newindvlist,oldindvlist,form)
	else:
		nextform = form[:]
	retform = [nextform[0]]
	for x in nextform[1:]:
		retform.append(bvarreplace(x, newbvarlist))
	return retform
	
def nfvlist(form):
	"""Return a list of variables with a non-free occurrence with repeats for distinct scopes"""
	retlist = []
	if type(form) is not list: 
		return []

	if type(form[0][1]) is list and len(form[0][1]) > 1:
		retlist = indvlist(form)
		for x in form[1:]:
			retlist.extend(nfvlist(x))
		return retlist

	for x in form[1:]:
		if type(x) is str: 
			pass

		elif x[0][0] == 48:
			state = 0
			for xi in x[1:]:
				if type(xi) is list: 
					if state == 1:
						state = 2
						retlist.extend(nfvlist(xi))
				elif symtype(xi) == 14:
					if state == 1:
						state = 2
				elif symtype(xi) == 10:
					if state == 0: 
						retlist.append(xi)
					elif state == 1:
						state = 2
				elif xi ==  ',':
					if state == 2:
						state = 0
				elif symtype(xi) == 3:
					if state == 0:
						state = 1
				else:
					raise "Scope error"
		else:
			retlist.extend(nfvlist(x))
	return retlist

def indvlist(form):
	retlist = []
	if type(form) is not list: 
		return retlist
	if len(form[0]) > 1 and type(form[0][1]) is list and len(form[0][1]) > 1:
		for x in form[0][1][1:]:
			retlist.append(form[x])
		return retlist
	for x in form[1:]:
		if type(x) is str: 
			pass
		elif x[0][0] == 48:
			state = 0
			for xi in x[1:]:
				if type(xi) is list: 
					if state == 1:
						state = 2
				elif symtype(xi) == 14:
					if state == 1:
						state = 2
				elif symtype(xi) == 10:
					if state == 0: 
						retlist.append(xi)
					elif state == 1:
						state = 2
				elif xi ==  ',':
					if state == 2:
						state = 0
				elif symtype(xi) == 3:
					if state == 0:
						state = 1
				else:
					print "form = ", form
					print "x = ", x
					print "xi = ", xi
					raise "Scope error"
	return retlist

def indvsubst(inlist, outlist, form):
# It is expected that outlist = indvlist(form)
# and that inlist consists of fresh bound variables
	if type(form) is not list: 
		if inlist == []:
			return form
		else:
			print form
			raise "Not a bound variable form."
	newform = [form[0]]
	if len(form[0]) == 1:
		print "form = ", form
	if type(form[0][1]) is list and len(form[0][1]) > 1:
		definiendum = mathdb[MD_DEFS][form[1]][form[0][1][0]]
		indeflist = indvlist(definiendum)
		ibvlist = definiendum[0][1][1:]
		for n in range(1,len(form)):
			if type(definiendum[n]) is str: 
				if n in ibvlist and form[n] in outlist:
					newform.append(inlist[outlist.index(form[n])])
				else: 
					newform.append(form[n])
			elif definiendum[n][0][0] in [42,43]:
				subinlist = []
				suboutlist = []
				for m in ibvlist:
					if definiendum[m] in definiendum[n]: 
						if form[m] in outlist:
							subinlist.append(inlist[outlist.index(form[m])])
							suboutlist.append(form[m])
						else:
							raise "Bound variable error"
				newform.append(subst(subinlist,suboutlist,form[n]))
			else:
				newform.append(form[n])
		return newform
	for x in form[1:]:
		if type(x) is str: 
			if x in outlist:
				newform.append(inlist[outlist.index(x)])
			else:	
				newform.append(x)
		elif x[0][0] == 48:
			accum_inlist = []
			accum_outlist = []
			new_inv = ''
			new_outv = ''
			newscope = [x[0]]
			state = 0
			for xi in x[1:]:
				if type(xi) is list:
					if state == 1:
						state = 2
						newscope.append(subst(accum_inlist,accum_outlist,xi))
					elif state == 2:
						newscope.append(subst(accum_inlist,accum_outlist,xi))
				elif symtype(xi) == 14: 
					if state == 1:
						state = 2
						newscope.append(xi)
					elif state == 2:
						newscope.append(xi)
				elif symtype(xi) == 10: 
					if state == 0:
						if new_outv:
							accum_outlist.append(new_outv)
						if new_inv:
							accum_inlist.append(new_inv)
						if xi in outlist:
							new_outv = xi
							new_inv = inlist[outlist.index(xi)]
							newscope.append(new_inv)
						else:
							new_outv = ''
							new_inv = ''
							newscope.append(xi)
					elif state == 1:
						state = 2
						newscope.append(xi)
					elif state == 2:
						newscope.append(xi)
				elif xi == ',':
					if state == 2:
						state = 0
					newscope.append(xi)  
				elif symtype(xi) == 3:
					if state == 0:
						state = 1
					newscope.append(xi)
				else:
					raise "Scope error"
			newform.append(newscope)
		else:
			newform.append(subst(inlist,outlist,x))
	return newform

def accvlist(form):
# Only called on notarian forms
	retlist = []
	if type(form) is not list: 
		return retlist
	for x in form[1:]:
		if type(x) is str: 
			pass
		elif x[0][0] == 48:
			state = 0
			newbv = ''
			accum = []
			for xi in x[1:]:
				if type(xi) is list: 
					if state == 1:
						state = 2
					retlist.extend(nblist(xi,accum))
				elif symtype(xi) == 14:
					if state == 1:
						state = 2
				elif symtype(xi) == 10:
					if state == 0:
						if newbv:
							accum.append(newbv)
						newbv = xi
					if state == 1:
						state = 2
					if state == 2:
						if xi not in accum:
							retlist.append(xi)
				elif xi ==  ',':
					if state == 2:
						state = 0
				elif symtype(xi) == 3:
					if state == 0:
						state = 1
				else:
					print "form = ", form
					print "x = ", x
					print "xi = ", xi
					raise "Scope error"
	return retlist

def subst(inlist,outlist,pexp): 
	""" Indiscriminate string substitution """
	if type(pexp) is list:
		r = []
		for t in pexp:
			r.append(subst(inlist,outlist,t))
		return r
	elif type(pexp) is str:
		if pexp in outlist:
			return inlist[outlist.index(pexp)]
		else:
			return pexp
	else:  #Numbers don't get trashed
		return pexp

def cmop(form):
	if type(form) is not list: return ''
	if len(form[0])< 3:return ''
	if form[0][2] != -1: return ''
	return form[2]


def nblist(form, boundlist = []):
	indvars = indvlist(form)
	if type(form) is str: 
		if form in boundlist:
			return []	
		if symtype(form) in [10,11,12,13]:
			if pattern.bvar.match(form):
				return []
			return [form]
		return []	
#else	 type(form) is list: 
	assert type(form[0]) is list
	if form[0] == [11] :return form[1:]
	retlist = []
	if type(form[0][1]) is list and len(form[0][1]) > 1:
		definiendum = mathdb[MD_DEFS][form[1]][form[0][1][0]]
		ibvlist = definiendum[0][1][1:]
		addvars = []
		for n in range(1,len(form)):
			if type(definiendum[n]) is str: 
				if n in ibvlist:
					pass
				elif symtype(definiendum[n]) in [10,11]:
					addvars.append(definiendum[n])
			elif definiendum[n][0][0] in [42,43]:
				suboutlist = []
				for m in ibvlist:
					if definiendum[m] in definiendum[n]:
						suboutlist.append(form[m])
				addvars.extend(nblist(form[n],boundlist + suboutlist))
			else:
				addvars.extend(nblist(form[n]),boundlist)
		for x in addvars:
			if x not in retlist:
				retlist.append(x)
		return retlist 
	for x in form[1:]:
		if x[0][0] == 48:
			state = 0
			addvars = []
			accum = []
			newbv = ''
			for xi in x[1:]:
				if type(xi) is list: 
					if state == 1:
						state = 2
						addvars = addvars + nblist(xi, boundlist + accum)
					elif state == 2:
						addvars = addvars + nblist(xi, boundlist + accum)
				elif symtype(xi) == 14:
					if state == 1:
						state = 2
				elif symtype(xi) in [10,11,12,13]:
					if state == 0:
						if newbv:
							accum.append(newbv)
						newbv = xi
					elif state == 1:
						state = 2
						if xi not in boundlist:
							if x not in accum:
								addvars.append(xi)
					elif state == 2:
						if xi not in boundlist:
							if x not in accum:
								addvars.append(xi)
				elif xi == ',':
					if state == 2:
						state = 0
				elif symtype(xi) == 3 :
					if state == 0:
						state = 1
		else:
			addvars = nblist(x, boundlist + indvars)
		for t in addvars:
			if t not in retlist:
				retlist.append(t)
	return retlist

def getnote(linetail, verbose=False):
	linelist = getnotelines(linetail,verbose)
#	print linelist
	if not linelist:
		return []
	
	mathparsed_note = linelist.pop()
	workparse = deep(mathparsed_note)
	if workparse[0][0] in [10,11]:
	  workparse = workparse[1]

	steplist = [[[]]]
	tempref = ''
	for t in linelist:
		if t[0] :
			steplist[-1].append(tempref)
			tempref = t.pop()
			steplist.append(t)
		else:
			if tempref:
				print "Lost reference found near",linetail[2],":", tempref
			tempref = t.pop()
			steplist[-1].append(t[1])
	steplist[-1].append(tempref)

	if len(steplist) == 1:
		textfragments = steplist[0][1:-1]
		thisref = steplist[0][-1]
		return [1, varlist(workparse), workparse, textfragments, thisref]
	
	chain = buildchain(steplist)
	if chain == []:
		print "Note multi-line note error" , linetail[0]
		print linetail[2]
		return 0
	chain.append(varlist(workparse))
	return chain

# The following function returns a list whose
# last element is just the mathparse of the note
# and all preceeding elements are triples
# [number, fetched note text, fetched reference text]
# The number is positive if a transitive parse
# is indicated. 
# An empty return indicates an error.

def getnotelines(linetail,verbose=False):
	precedence = mathdb[MD_PRECED]
	mode = [2]
	parsetree = []

	linelist=[]
	lenstepconnector = -1
	thisref = ''
	stepcount = 0
	linecopy = linetail[0].lstrip()
	while 1:
		TeXdollars = pattern.TeXdollar.search(linecopy)
		dollar_spot = linecopy.find('$')
		by_spot = linecopy.find('\\By')
		tokenm = pattern.token.match(linecopy)
		if mode[0] == 1:
			if by_spot != -1:
				thisref = linecopy[by_spot + 3:].strip()
			elif thisref:
				thisref = thisref + linecopy
			else:
				thisref = ''
			linelist.append([lenstepconnector, newstuff, thisref])
			parsed_item = parsetree[0]
			linelist.append(parsed_item)
			return linelist
		elif mode[0] == 2 and not tokenm:
			newtag = []
#			print "linelist = ", linelist
#			print "lenstepconnector = ", lenstepconnector
#			print "newstuff = ", newstuff
#			print "thisref = ", thisref
#			print "linecopy = ", linecopy
#			print "dollar_spot = ", dollar_spot
#			print "newtag = ", newtag
#			print "TeXdollars = ", TeXdollars 
			linecopy = linecopy[dollar_spot+1:]
			linetail[0] = linecopy
			mode[0] = 3
		elif mode[0] == 2:
#			tokenm = pattern.token.match(linecopy)
			newtag = optag(linecopy,parsetree)
			token = tokenm.group(2)
			nexttokenlen = len(token)
			if lenstepconnector != -1:
				linelist.append([lenstepconnector, newstuff, thisref])
			thisref = ''
			if TeXdollars: 
				newstuff = linecopy[:TeXdollars.start(1)]
			else:
				newstuff = linecopy
			linetail[0] = linecopy
			if newtag == []:
				lenstepconnector = []		
			elif parsetree == []:
				print "Error in multi-line note"
				return []
			elif opcoords(parsetree,precedence[token]) == (0,0): #Check for preceding op
				if len(linelist) > 2:
					reverse_linelist = linelist[:]
					reverse_linelist.reverse()
					for t in reverse_linelist:
						if t[0]!=[]:
							if t[0][1] < newtag[1] or\
                        (t[0][1] == newtag[1] and t[0][2] < newtag[2]):
								break
							else:
								if newtag[3] == '\\c':
									print "Consider using `True implies ...'"
								print "Note parse error line: ", str(linetail[2]),linetail[0]
								return [] 
				lenstepconnector = []
			else:
				lenstepconnector = newtag
		
			mathparse(mode,linetail,parsetree)
		elif mode[0] == 3:
			if not TeXdollars: 
				by_spot = linecopy.find('\\By')
				if by_spot == -1:
					thisref = thisref + ' ' + linecopy.strip()
				else:
					thisref = thisref + ' ' + linecopy[by_spot+3:].strip()
			linetail[0] = linecopy
			notemargin(mode,linetail)
		elif mode[0] == 4:
			print "Note parse error line: ", str(linetail[2]),linetail[0]
			return [] 
		elif mode[0] == 5:
			print "Note fails to terminate: ", str(linetail[2]),linetail[0]
			return [] 
		linecopy = linetail[0].lstrip() 
		if not linecopy and mode[0] != 1:
			getline(linetail,verbose)
			linecopy =  linetail[0].lstrip()


def optag(line,node):
	precedence = mathdb[MD_PRECED]
	original_len = len(line)
	tokenm = pattern.token.match(line)
	token = tokenm.group(2)
	tokens = []
	precedence_list = []
	while symtype(token) < 4:
		precedence_list.append(precedence[token])
		if precedence[token] != precedence_list[0]:
			break
		tokens.append(token)
		nexttokenlen = len(token)
		line = line[nexttokenlen:]
		line = line.lstrip()
		tokenm = pattern.token.match(line)
		if tokenm:
			token = tokenm.group(2)
		else:
			break
	if len(tokens)== 0:
		return []
	minimum_precedence = min(precedence_list)
	if minimum_precedence not in [2,4,6]:
		return []
	if len(tokens) == 1 :
		return [len(tokens[0]),len(node),minimum_precedence,tokens[0]]
	elif len(tokens) > 1:
		return [original_len -len(line),len(node),minimum_precedence,tuple(tokens)]

def buildchain(steplist):
	firstline =  ' '.join(steplist[0][1:-1])
	treesg = []
	mode = [2]
	mathparse(mode,[firstline],treesg)
	chain = [[parenclose(treesg),[],steplist[0][1:-1], steplist[0][-1]]]
	stack = [[subst([],[],treesg), 0, 0]]
	treedl = subst([],[],treesg)
	num_steps = len(steplist)
	steplist.append([[0,0,0,'']])
	for k in range(1,num_steps):
		tran_tag = steplist[k][0]	
		next_tran_tag = steplist[k+1][0]	
		(dpth,prcd) = [tran_tag[1], tran_tag[2]] 
		(next_dpth, next_prcd) = [next_tran_tag[1], next_tran_tag[2]]
		op = tran_tag[3]
		oplen = tran_tag[0]
		op = steplist[k][1][:oplen]
		finish = steplist[k][1][oplen:] + ' ' + ' '.join(steplist[k][2:-1])
		if type(op) is not str: 
			print "step = ", steplist[k]
			print "oplen = ", oplen
			print "op = ", steplist[k][1][:oplen]
		mathparse(mode,[op],treesg)
		mathparse(mode,[op],treedl)
		deltadelete(treedl,prcd)
		ok = sigmarevise(treesg,prcd)
		if ok == 0:
			return []
		depth = len(treedl)
		splitlist = finish.split(')')
		len_splitlist = len(splitlist)
		j = 0
		mathparse(mode,[splitlist[j]],treedl)
		mathparse(mode,[splitlist[j]],treesg)
		while j + 1 < len_splitlist and len(treedl) > depth:
			j = j + 1
			mathparse(mode,[')' + splitlist[j]],treedl)
			mathparse(mode,[')' + splitlist[j]],treesg)
		if j + 1 < len_splitlist:
			splitlist = splitlist[j+1:]
		else:
			splitlist = []

		pr = treesg	
		qr = treedl
		pq = pr
		link = [parenclose(treedl),[], steplist[k][1:-1],steplist[k][-1]]
		chain.append(link)
		
		while (dpth, prcd)  >=  (next_dpth, next_prcd):
			old_dpth = dpth
			old_prcd = prcd
			if not stack:
				break
			[last_treesg, dpth, prcd] = stack.pop()
			qr = subst([],[],treedl)
			pq = subst([],[],last_treesg)
			(op_spot, op_len) = opcoords(pr,old_prcd)
			
			if (op_spot,op_len) == (0,0):
				break	
			op = pr[-1][op_spot:op_spot + op_len]
			finish = pr[-1][op_spot + op_len:]
			tail_prcd= pr[-1][0][1] 

			paradecrop(last_treesg[-1], old_prcd)
			last_treesg[-1].extend(op)
			pr = subst([],[], last_treesg)
			sigmarevise(pr,old_prcd)
			pr[-1].extend(finish)
			pr[-1][0][1] = tail_prcd
			if old_dpth > next_dpth:
				while len(pr) >= next_dpth > 0 and len(pr) > 1 and len(splitlist) > 0:
					mathparse(mode,[')' + splitlist[0]], pr)
					mathparse(mode,[')' + splitlist[0]], treesg)
					splitlist = splitlist[1:]
					
				
			link[1].append([parenclose(pr),parenclose(pq),parenclose(qr)])
			treedl = pr

		if splitlist:
			close_off = ')' + ')'.join(splitlist)
			mathparse(mode,[close_off],treedl)
			mathparse(mode,[close_off],treesg)
		stack.append([subst([],[],treedl),dpth, prcd])
	return chain

def parenclose(parsetree):
	copy_tree = subst([],[],parsetree)
	mode = [2]
	while mode[0] ==2:
		mathparse(mode,[')'],copy_tree)
	if mode[0] == 4:
		return []
	return copy_tree[0]

	
def deltadelete(tree,precedence):
	node = tree[-1]
	(index, length) = opcoords(tree,precedence)
	for n in range(length+1):
		del node[index-1]
	return 

def sigmarevise(tree,precedence):
	node = tree[-1]
	(index, length) = opcoords(tree,precedence)
	if length == 1:
		nexusop = node[index][1]
	else:
		nexus_list = []
		for i in range(length):
			nexus_list.append(node[index + i][1])
		nexusop = tuple(nexus_list)
	resultop=transopswap(nexusop,node[index+length+1][1])
	if resultop == 0 :
		if not mathdb[MD_RSFLG]:
			pass
#			print "Transitivity not established."
	elif type(resultop) is str: 
		node[-1][1] = resultop
	else:
		resultnodes = []
		for op in resultop:
			resultnodes.append([node[-1][0], op ,node[-1][2]])
		node[-1:] = resultnodes
	for n in range(length+1):
		del node[index]
	return

def opcoords(parsetree,precedence ):
	node = parsetree[-1]
#	precedence = node[0][1]

	i = len(node) -1
	while  i > 0 :
		i = i - 1
		if opnodep(node[i],precedence) and\
		not(i > 0 and opnodep(node[i-1],precedence)): 
			break
	if i == 0:
		return (0,0)
	k = 1
	while   i + k  < len(node) and opnodep(node[i + k],precedence) :
		k = k + 1 
	return (i,k)

def opnodep(node,precedence): 
	return (type(node) is list and type(node[0]) is list and 
   node[0][0] < 4 and node[2]== precedence) 

def opnodep(node,precedence): 
	return (type(node) is list and type(node[0]) is list and 
   node[0][0] < 4 and node[2]== precedence) 

def transopswap(op1, op2):
	transitive_ops = mathdb[MD_TROPS]
	trans_mult = mathdb[MD_TRMUL]
	precedence = mathdb[MD_PRECED]
	if op1 == op2: 
		if op1 in transitive_ops:
			return op2
		else:
			return 0
	if op1 == '\\ident':
		if type(op2) is str: 
			if symtype(op2)in [1,2,3] and precedence[op2] == 6: 
				return op2
			else:
				return 0
		else:
			for x in op2:
				if symtype(x) not in [1,2,3] or precedence[x] != 6:
					return 0
			return op2

	if op2 == '\\ident':
		if type(op1) is str: 
			if symtype(op1)in [1,2,3] and precedence[op1] == 6: 
				return op1
			else:
				return 0
		else:
			for x in op1:
				if symtype(x) not in [1,2,3] or precedence[x] != 6:
					return 0
			return op1
	if op1 == '=':
		if type(op2) is str: 
			if symtype(op2) == 3 and precedence[op2] == 6: 
				return op2
			else:
				return 0
		else:
			for x in op2:
				if symtype(x) != 3 or precedence[x] != 6:
					return 0
			return op2
	if op2 == '=':
		if type(op1) is str: 
			if symtype(op1) == 3 and precedence[op1] == 6: 
				return op1
			else:
				return 0
		else:
			for x in op1:
				if symtype(x) != 3 or precedence[x] != 6:
					return 0
			return op1

	if op1 == '\\Iff' and op2 == '\\c': return op2
	if op2 == '\\c' and op2 == '\\Iff': return op1
	if (op1,op2) in trans_mult.keys():
		return trans_mult[(op1,op2)]
	else:
		return 0

def definition_check(unparsed_exp):
	assert type(unparsed_exp) is str
	parsed_exp = stringparse(unparsed_exp)
	if type(parsed_exp) is list and parsed_exp[0][0] in [41,51] and \
		type(parsed_exp[2]) is list and len(parsed_exp[2]) == 4:
#      parsed_exp[2][0][0]  == 45 :
		definiendum = parsed_exp[2][1]
		definor = parsed_exp[2][2]
		definiens = parsed_exp[2][3]
		left_vars = set(nblist(definiendum)) 
		right_vars = set(nblist(definiens))
#		if definor not in ['\\ident','\\Iff']:
		if symtype(definor) not in [1,2]:
			return "Definitions must have the form (p \\Iff q) or (x \\ident y)."
		if left_vars == right_vars: 
			return	
		elif right_vars - left_vars:
			error_message = "Dropped variables: " 
			for x in (right_vars - left_vars):
				error_message = error_message + " " + x
			return error_message 
		elif left_vars - right_vars:
			error_message = "Useless variables: " 
			for x in (left_vars - right_vars):
				error_message = error_message + " " + x
			return error_message 
		if parsed_exp[0][0] == 51:
#			print "New form:"
#			print "definiendum = ", parsed_exp[2][1]
			pass
	else:
		print "parsed_exp[0][0] = ",parsed_exp[0][0]
		print "len(parsed_exp[2]) = ",len(parsed_exp[2]) 
		print "definiendum = ", parsed_exp[2][1]
		print "definor = ", parsed_exp[2][2]
		print "definiens = ", parsed_exp[2][3]
		return "Definitions must have the form (p \\Iff q) or (x \\ident y)."
	print definiendum
	ok = raw_input()
	return ok


def translate(linelist, userdict = []):
	""" Use the translation macros stored in the math data base
	 to translate the list of strings in linelist."""
	if userdict == []:
		userdict = mathdb[MD_MACR]

	def usedict(x):  # Define dictionary using function 
		if x.group(2):
			return ''
		y = x.group(3)
		return '\\' + userdict.get(y,y)

	newlines = []
	indollars = False
	for r in linelist:
		comment_match =  re.search(pattern.TeXcomment,r)
		if comment_match:
			comment = r[comment_match.start(1):]
			r = r[:comment_match.start(1)]
		else:
			comment = ''
	
		splitlist = re.split(pattern.TeXdollar,r)
		assert len(splitlist) % 2 == 1
		newr = ''
		for jj in range(0,len(splitlist),2):
			if not indollars:
				newr = newr + splitlist[jj]
			else:
				newr = newr + re.sub(pattern.alphacontrolseq_or_skip,usedict,splitlist[jj])
			if jj + 1 == len(splitlist):
				continue
			newr = newr + splitlist[jj + 1]
			indollars = not indollars
					
		newr = newr + comment
		newlines.append(newr)
	return newlines 
	
def pprint(exp, depth = 0):
	if type(exp) is str: 
		print depth * '  ' + exp
	elif len(str(exp)) + depth *2< 60: 
		print depth * '  ' + str(exp)
	else:
		print depth * '  ' + str(exp[0])
		for t in exp[1:]:
			pprint(t, depth + 1) 
	return


def freshsub(vars_pexp,takenvars,fixedvars = []):	
	global newvarnum
	newvlist = []
	oldvlist = []
	for t in vars_pexp :
		if t in fixedvars:
			pass
		elif t in takenvars:
			oldvlist.append(t)
			newvar = t
			while newvar in takenvars:
				newvarnum = newvarnum + 1
				st = symtype(t)
				arity = mathdb[MD_ARITY]
				pf =  '_{' + ("%d"%newvarnum) + '}'
				if st == 10: 
					newvar = 'v' + pf
				else:
					r = arity[t]
					if st == 11:
						newvar = '\\q^{0}' + pf 
					elif st == 12:
						newvar = '\\w^{' +  ("%d"%r)  + '}' + pf
					elif st == 13:
						newvar = '\\q^{' +  ("%d"%r)  + '}' + pf 
			takenvars.append(newvar)
			newvlist.append(newvar)
		else:
			takenvars.append(t)
	return [newvlist, oldvlist]


############################################################
#
#  Get properties from file and store in db
#
############################################################

def readprops(propfilename, db):

#	print "len db = ", len(db)
#	print "propfilename = ", propfilename
	f = open(propfilename,"r")
	line_list = f.readlines()
	f.close()

	transitive_ops = [] 
	trans_mult = {}
	commutative_associative_ops = []

	commutative_ops = []
	associative_ops = []
	chain_triplets = []

	linetail = [line_list[0],0,1,line_list]
	r = linetail[0]
	while r:
		dollarm = pattern.dollar.match(r)
		if dollarm:
			dollar_spot= dollarm.start(1)
			linetail[0] = dollarm.group(1) 
			reffed_item = getformula(linetail)
			if not reffed_item:
				print "Error in file",propfilename," :", linetail[1]
				raise SystemExit
			parsed_item = deep(reffed_item[2])
			op = detect_commutative_op(parsed_item)	
			if op:
				commutative_ops.append(op)
			op = detect_associative_op(parsed_item)	
			if op:
				associative_ops.append(op)
			optrip = detect_chain(parsed_item)
			if optrip:
				chain_triplets.append(optrip)
		else:
			getline(linetail)
		r = linetail[0]
	
	#
	commutative_associative_ops = []
	c = []
	
	for x in commutative_ops:
		if x in associative_ops:
			commutative_associative_ops.append(x)
		else:
			c.append(x)
	commutative_ops = []
	
	transitive_ops = ['\\ident','\\Iff','=']
	transitive_mult = {}
	for trip in chain_triplets:
		if trip[0] == trip[1] == trip[2]:
			if trip[0] not in transitive_ops:
				transitive_ops.append(trip[0])	
	for trip in chain_triplets:
		if trip[0] in transitive_ops and trip[1] in transitive_ops and trip[2] in transitive_ops:
			if trip[0] != trip[2] or trip[1] != trip[2]:
				if (trip[0],trip[1]) not in transitive_mult.keys():
					transitive_mult[(trip[0],trip[1])] = trip[2]
	
	#precedence = syntdb[1]
	db[MD_TROPS] = transitive_ops 
	db[MD_TRMUL] = transitive_mult 
	db[MD_CAOPS] = commutative_associative_ops
#	print "Commutative Associative Ops", commutative_associative_ops 
#	print "Commutative Ops", commutative_ops
#	print "Transitive Ops", transitive_ops
	
	############################################################
	
	if len(db) > MD_THMS:
		db[MD_THMS] = [] 
	else:
#		print "Adding theorems from ", propfilename
		db.append([])
		db.append([])
				
	############################################################
	#
	#  Re-parse the properties theorems
	#
	############################################################
	
	mathdb = db
	
	linetail = [line_list[0],0,1,line_list]
	theorems = []
	r = linetail[0]
	while r:                   # Begin properties file pass
		if r[0] == '$':
			linetail[0] = r[1:]
			next_file_entry = getformula(linetail)
			if next_file_entry:
				wp = deep(next_file_entry[2])
			else:
				print "Error in properties file, line: ", linetail[1]
				raise SystemExit
			theorems.append([wp, varlist(wp)])
		getline(linetail)
		r = linetail[0] 
		
	db[MD_THMS] = theorems
	db[MD_PFILE] = propfilename
	print propfilename
	return db
	
	
#
################################################################
#
#             Read Properties Theorems
#
############################################################

def binopnp(exp):	
	if type(exp) is str: return ''
	if len(exp) < 4: return ''
	if type(exp[2]) is list : return ''
	for k in range(2, len(exp) -1):
		if type(exp[k]) is list :return ''
		if symtype(exp[k]) not in [1,2,3]: return ''
	if len(exp) == 4:
		return exp[2]
	return exp[2:-1]
	
def detect_commutative_op(exp):
	if binopnp(exp) not in ['\\ident','\\Iff']:
		return ''
	op1 = binopnp(exp[1])
	if not('' != op1 == binopnp(exp[3])):
		return ''
	if exp[1][1] != exp[3][-1] :
		return ''
	if exp[3][1] != exp[1][-1]:
		return ''
	if exp[1][1] == exp[1][-1]:
		return ''
	if type(exp[1][1]) is list  or type(exp[1][-1])is list :
		return ''
	if not(symtype(exp[1][1]) == symtype(exp[1][-1]) in [10,11]):
		return ''
	return op1 

def detect_chain(exp):
	if binopnp(exp) != '\\c':
		return ''
	if binopnp(exp[1]) != '\\And':
		return ''
	op1 = binopnp(exp[1][1])
	op2 = binopnp(exp[1][3])
	op3 = binopnp(exp[3])
	if op1 == '':
		print "Null", exp[1][1]
	if op2 == '':
		print "Null", exp[1][3]
	if op3 == '':
		print "Null", exp[3]
	if op1 == '' or op2 == '' or op3 == '':
		return ''
	if op3 != op1 and op3 != op2:
		return ''
	if exp[1][1][3] != exp[1][3][1]:
		return ''
	if exp[1][1][1] != exp[3][1]:
		return ''
	if exp[1][3][3] != exp[3][3]:
		return ''
	if exp[3][1] == exp[1][1][3] or exp[3][1] == exp[3][3] or exp[1][3][3] == exp[3][1]:
		return ''
	if type(exp[3][1]) is not str :
		return ''
	if type(exp[1][3][1]) is not str :
		return ''
	if type(exp[3][3]) is not str :
		return ''
	if not(symtype(exp[1][1][1])== symtype(exp[1][1][3]) in [10,11]):
		return ''
	if not(symtype(exp[1][1][1])== symtype(exp[3][3]) in [10,11]):
		return ''
	return [op1,op2,op3]

def detect_associative_op(exp):
	if binopnp(exp) not in ['\\ident','\\Iff']: return ''
	op1 = binopnp(exp[1])
	if not('' != op1 == binopnp(exp[3])): return ''
	op11 = binopnp(exp[1][1])
	if op11: 
		if op11 != op1: return ''
		if op11 != binopnp(exp[3][-1]): return ''
		if type(exp[1][-1]) is not str : return ''
		if type(exp[3][1]) is not str  : return ''
		if type(exp[1][1][-1]) is not str :return ''
		if exp[1][1][1] != exp[3][1]: return '' 
		if exp[1][1][-1] != exp[3][-1][1]: return '' 
		if exp[1][-1] != exp[3][-1][-1]: return '' 
		if symtype(exp[1][1][1]) not in [10,11]: return '' 
		if symtype(exp[1][1][-1]) not in [10,11]: return '' 
		if symtype(exp[1][-1]) not in [10,11]: return '' 
		return op1 
	else:
		op31 = binopnp(exp[3][1])
		if op31 != op1:  return ''
		if op31 != binopnp(exp[1][-1]): return ''
		if type(exp[3][-1]) is not str : return ''
		if type(exp[1][1]) is not str : return ''
		if type(exp[3][1][-1]) is not str :return ''
		if exp[1][1] != exp[3][1][1]: return '' 
		if exp[1][-1][1] != exp[3][1][-1]: return '' 
		if exp[1][-1][-1] != exp[3][-1]: return '' 
		if symtype(exp[1][1]) not in [10,11]: return '' 
		if symtype(exp[1][-1][1]) not in [10,11]: return '' 
		if symtype(exp[1][-1][-1]) not in [10,11]: return '' 
		return op1
*/
  	
}