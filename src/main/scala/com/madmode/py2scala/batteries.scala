package com.madmode.py2scala


/**
 * batteries -- python standard library
 */
object batteries {
  import com.madmode.py2scala.{__builtin__ => b }
  import com.madmode.py2scala.__builtin__.TODO

  /**
  Configuration file parser.

A setup file consists of sections, lead by a "[section]" header,
and followed by "name: value" entries, with continuations and such in
the style of RFC 822.

The option values can contain format strings which refer to other values in
the same section, or values in a special [DEFAULT] section.

For example:

    something: %(dir)s/whatever

would resolve the "%(dir)s" to the value of dir.  All reference
expansions are done late, on demand.

Intrinsic defaults can be specified by passing them into the
ConfigParser constructor as a dictionary.

class:

ConfigParser -- responsible for parsing a list of
                configuration files, and managing the parsed database.

    methods:

    __init__(defaults=None)
        create the parser and specify a dictionary of intrinsic defaults.  The
        keys must be strings, the values must be appropriate for %()s string
        interpolation.  Note that `__name__' is always an intrinsic default;
        its value is the section's name.

    sections()
        return all the configuration section names, sans DEFAULT

    has_section(section)
        return whether the given section exists

    has_option(section, option)
        return whether the given option exists in the given section

    options(section)
        return list of configuration options for the named section

    read(filenames)
        read and parse the list of named configuration files, given by
        name.  A single filename is also allowed.  Non-existing files
        are ignored.  Return list of successfully read files.

    readfp(fp, filename=None)
        read and parse one configuration file, given as a file object.
        The filename defaults to fp.name; it is only used in error
        messages (if fp has no `name' attribute, the string `<???>' is used).

    get(section, option, raw=False, vars=None)
        return a string value for the named option.  All % interpolations are
        expanded in the return values, based on the defaults passed into the
        constructor and the DEFAULT section.  Additional substitutions may be
        provided using the `vars' argument, which must be a dictionary whose
        contents override any pre-existing defaults.

    getint(section, options)
        like get(), but convert value to an integer

    getfloat(section, options)
        like get(), but convert value to a float

    getboolean(section, options)
        like get(), but convert value to a boolean (currently case
        insensitively defined as 0, false, no, off for False, and 1, true,
        yes, on for True).  Returns False or True.

    items(section, raw=False, vars=None)
        return a list of tuples with (name, value) for each option
        in the section.

    remove_section(section)
        remove the given file section and all its options

    remove_option(section, option)
        remove the given option from the given section

    set(section, option, value)
        set the given option

    write(fp)
        write the configuration state in .ini format
    */
  object ConfigParser {

    import __builtin__._

    def _default_dict = new Dict[String, String]()
    val DEFAULTSECT = "DEFAULT"
    val MAX_INTERPOLATION_DEPTH = 10

    // exception classes
    /**
    Base class for ConfigParser exceptions.
      */
    class Error(msg: String = "") extends Exception(msg)

    /**
    Raised when no section matches a requested option.
      */
    case class NoSectionError(section: String) extends Error("No section: %r" % section)

    /**
    Raised when a section is multiply-created.
      */
    class DuplicateSectionError(section: String) extends Error("Section %r already exists" % section)

    /**
    A requested option was not found.
      */
    case class NoOptionError(option: String, section: String) extends Error("No option %r in section: %r" %(option, section))

    /**
    Base class for interpolation-related exceptions.
      */
    class InterpolationError(option: String, section: String, msg: String) extends Error(msg)

    /**
    A string substitution required a setting which was not available.
      */
    class InterpolationMissingOptionError(option: String, section: String, rawval: Any, reference: Any)
      extends InterpolationError(section, option,
        "Bad value substitution:\n\tsection: [%s]\n\toption : %s\n\tkey    : %s\n\trawval : %s\n"
          %(section, option, reference, rawval))

    /**
    Raised when the source text into which substitutions are made
  does not conform to the required syntax.
      */
    //class InterpolationSyntaxError extends InterpolationError

    /**
    Raised when substitutions are nested too deeply.
      */
    //class InterpolationDepthError(option: Any, section: Any, rawval: Any) extends InterpolationError

    /**
    Raised when a configuration file does not follow legal syntax.
      */
    //class ParsingError(filename: Any) extends Error

    /**
    Raised when a key-value pair is found before any section header.
      */
    //class MissingSectionHeaderError(filename: Any, lineno: Any, line: Any) extends ParsingError

    class RawConfigParser(defaults: Dict[String, String] = null,
                          dict_type: (() => Dict[String, String]) = _default_dict _,
                          allow_no_value: Boolean = False) {

      /**
      Return a list of section names, excluding [DEFAULT]
        */
      def sections(): Seq[String] = TODO // mutable?

      /**
      Create a new section in the configuration.

    Raise DuplicateSectionError if a section by the specified name
    already exists. Raise ValueError if name is DEFAULT or any of it's
    case-insensitive variants.
        */
      def add_section(section: String) = TODO

      /**
      Indicate whether the named section is present in the configuration.

    The DEFAULT section is not acknowledged.
        */
      def has_section(section: String): Boolean = TODO

      /**
      Return a list of option names for the given section name.
        */
      def options(section: String): Seq[String] = TODO

      /**
      Read and parse a filename or a list of filenames.

    Files that cannot be opened are silently ignored; this is
    designed so that you can specify a list of potential
    configuration file locations (e.g. current directory, user's
    home directory, systemwide directory), and all existing
    configuration files in the list will be read.  A single
    filename may also be given.

    Return list of successfully read files.
        */
      def read(filenames: Iterable[String]) = TODO

      /**
      Like read() but the argument must be a file-like object.

    The `fp' argument must have a `readline' method.  Optional
    second argument is the `filename', which if not given, is
    taken from fp.name.  If fp has no `name' attribute, <???> is
    used.
        */
      def readfp(fp: File, filename: String = null) = TODO

      // hmm... Option[String] somehow?
      def get(section: String, option: String): String = TODO

      def items(section: String): Dict[String, String] = TODO

      def getint(section: String, option: String): Int = TODO

      def getfloat(section: String, option: String): Float = TODO

      def getboolean(section: String, option: String): Boolean = TODO

      def optionxform(optionstr: String): String = TODO

      /**
      Check for the existence of a given option in a given section.
        */
      def has_option(section: String, option: String): Boolean = TODO

      /**
      Set an option.
        */
      def set(section: String, option: String, value: String = null) = TODO

      /**
      Write an .ini-format representation of the configuration state.
        */
      def write(fp: File) = TODO

      /**
      Remove an option.
        */
      def remove_option(section: String, option: String) = TODO

      /**
      Remove a file section.
        */
      def remove_section(section: String) = TODO
    }


    class ConfigParser() extends RawConfigParser {

      /**
      Get an option value for a given section.

    If `vars' is provided, it must be a dictionary. The option is looked up
    in `vars' (if provided), `section', and in `defaults' in that order.

    All % interpolations are expanded in the return values, unless the
    optional argument `raw` is true. Values for interpolation keys are


    The section DEFAULT is special.
        */
      def get(section: String, option: String, raw: Boolean = False, vars: Dict[String, String] = null): String = TODO

      // Update with the entry specific variables
      /**
      Return a list of tuples with (name, value) for each option
    in the section.

    All % interpolations are expanded in the return values, based on the
    defaults passed into the constructor, unless the optional argument
    `raw' is true.  Additional substitutions may be provided using the
    `vars' argument, which must be a dictionary whose contents overrides
    any pre-existing defaults.

    The section DEFAULT is special.
        */
      def items(section: String, raw: Boolean = False, vars: Dict[String, String] = null): Dict[String, String] = TODO
    }

    class SafeConfigParser() extends ConfigParser {
      /**
      Set an option.  Extend ConfigParser.set: check for string values.
        */
      override def set(section: String, option: String, value: String = null) = TODO

    }

    val __name__ = "ConfigParser"
  }


  object ast {
    sealed abstract class AST {
      def lineno: Int
    }
    abstract class expr extends AST {
      override def lineno = TODO
    }
    case class Str(s: String) extends expr
    abstract class stmt extends AST {
      override def lineno = TODO
    }
    case class Expr(value: expr) extends stmt
    abstract class Compound extends stmt { // made up name
      def body: Seq[stmt]
    }
    abstract class Decorated extends Compound {
      def decorator_list: Seq[expr]
    }
    type identifier = String
    type arguments = AST /* TODO */
    case class FunctionDef(name: identifier, args: arguments,
      body: Seq[stmt], decorator_list: Seq[expr]) extends Decorated
    case class ClassDef(name: identifier, bases: Seq[expr], body: Seq[stmt],
      decorator_list: Seq[expr]) extends Decorated
    sealed abstract class operator
    case class Add(left: expr, right: expr) extends operator
    case class Sub(left: expr, right: expr) extends operator

    def parse(code: String, filename: String): AST = TODO

    class NodeVisitor {
      def visit(t: AST): Unit = TODO
    }
  }

  object datetime {
    case class date(year: Int, month: Int, day: Int) {
      def strftime(fmt: String): String = TODO
    }
    object date {
      def today(): date = TODO
    }
    case class datetime(year: Int, month: Int, day: Int,
                        hour: Int = 0, minute: Int = 0, second: Int = 0, microsecond: Int = 0,
                        tzinfo: Option[tzinfo] = None) {
      def +(d: timedelta): datetime = TODO
    }
    object datetime {
      def now(): datetime = TODO
    }
    case class timedelta(days: Int=0, seconds: Int=0, microseconds: Int=0)
    abstract class tzinfo
  }

  object getpass {
    def getuser(): String = TODO
  }

  object json {
    def dumps(x: Any): String = TODO
  }

  object logging {
    val CRITICAL = 50
    val DEBUG = 10
    val ERROR = 40
    val FATAL = 50
    val INFO = 20
    val NOTSET = 0
    val WARN = 30
    val WARNING = WARN

    def basicConfig(level: Int): Unit = TODO
    def getLogger(which: String): Logger = TODO
    class Logger {
      def debug(msg: String, args: Any*): Unit = TODO
      def info(msg: String, args: Any*): Unit = TODO
      def warn(msg: String, args: Any*): Unit = TODO
      // cheat a little... args should be Any*
      def critical(msg: String, exc_info: (Any, Any, Any)=null): Unit = TODO
    }
    object config {
      def fileConfig(n: String) = TODO
    }
  }

  object os {
    //TODO
    //import java.nio.file.Files
    //import java.nio.file.Paths

    import batteries.stat.ST_MTIME

    def getenv(n: String): String = TODO
    def environ: b.Dict[String, String] = TODO

    def name: String = TODO

    def listdir(path: String): Seq[String] = TODO
    def mkdir(path: String) = TODO

    def stat(path: String): Map[Int, Long] = TODO /*{
      import com.madmode.py2scala.{ batteries => py }
      val mtime = Files.getLastModifiedTime(Paths.get(path))
      /* TODO fill in other slots */
      Map(ST_MTIME -> mtime.toMillis)
    }*/
    def popen(cmd: String): b.File = TODO

    object path {
      def isdir(path: String): Boolean = TODO
      def isfile(filename: String): Boolean = TODO // Files.exists(Paths.get(filename))
      def join(segments: String*): String = TODO
      // Paths.get(x).resolve(Paths.get(y)).toString
      def splitext(path: String): (String, String) = TODO
      def basename(path: String): String = TODO
      def exists(path: String): Boolean = TODO
    }
  }

  object pickle {
    def load(path: b.File): Object = TODO
    def dump(x: Any, f: b.File): Unit = TODO
  }

  object pkg_resources {
    def resource_filename(ctx: String, res: String): String = TODO
    def resource_string(ctx: String, res: String): String = TODO
  }

  object re {
    import scala.util.matching
    import java.util.regex.Matcher

    def compile(s: String): RegexObject = {
      new RegexObject(s)
    }
    def match_(pattern: String, repl: String): Match = TODO

    class RegexObject(regex: String) extends matching.Regex(regex) {
      def match_(s: String): Match = TODO /* {
        val m = this.pattern matcher s
        runMatcher(m)
        new JavaMatch(m)
      }*/
      def match_(s: String, offset: Int): Match = match_(s.drop(offset))
      
      def search(s: String): Match = {
        new ScalaMatch(this.findFirstMatchIn(s))
      }
    }

    trait Match {
      def test(): Boolean
      def group(i: Int): String
      def start(i: Int): Int
      def end(i: Int): Int
      def groups(): Seq[String]
    }
    implicit def test_matcher(m: Match): Boolean = m != null && m.test()

    class JavaMatch(impl: Matcher) extends Match {
      def test() = impl.matches()
      def start(i: Int) = TODO
      def end(i: Int) = impl.end(i)
      def group(i: Int) = impl.group(i)
      def groups() = 1 to impl.groupCount map impl.group
    }

    class ScalaMatch(impl: Option[matching.Regex.Match]) extends Match {
      def test() = !impl.isEmpty
      def group(i: Int) = impl.get.group(i)
      def start(i: Int) = TODO
      def end(i: Int) = TODO
      def groups() = {
        val m = impl.get
        1 to m.groupCount map m.group
      }
    }
  }

  object stat {
    val ST_MTIME = 8
  }

  object StringIO {
    case class StringIO(contents: String = "") extends b.File {
      override def read() = contents
      override def read(n: Int) = contents
      override def readline() = TODO
      override def readlines() = TODO
      override def write(s: String) = TODO
      override def flush() = {}
      override def close() {}
      override def iterator = TODO
    }

  }

  object sys {
    def argv: Vector[String] = TODO
    def stdout: b.File = TODO
    def exc_info(): (Any, Any, Any) = TODO
  }
  object tokenize {
    type Token = (Int, String, (Int, Int), (Int, Int), String)
    def generate_tokens(readline: (() => String)): Stream[Token] = TODO

    object TokenType extends Enumeration {
      type TokenType = Value
      val COMMENT = Value
    }
    import TokenType._
  }

  object urllib {
    abstract class addinfourl extends b.File {
      // def getinfo(): ...
    }
    def unquote(string: String): String = TODO
  }
  
  object urlparse {
    /**
     * Parse a query string given as a string argument (data of type application/x-www-form-urlencoded).
     * Data are returned as a list of name, value pairs.
     */
    def parse_qsl(qs: String,
        keep_blank_values: Boolean=false, strict_parsing: Boolean=false): Seq[(String, String)] = TODO
  }
  
  object uuid {
    class UUID
    def uuid4(): UUID = TODO
  }

  object warnings {
    class UserWarning extends Exception
    def warn[T <: Exception](
        message: String,
        category: Class[T]=classOf[UserWarning] /*,
        stacklevel:Int=1 */) = TODO
  }
  
  object wsgiref {
    type Application = {  // KLUDGE: this type isn't actually named
      def __call__(
            env: b.Dict[String, String],
            start_response: (String, Seq[(String, String)]) => Unit): Iterable[String]
    }
    object handlers {
      class CGIHandler {
        def run(app: Application): Unit = TODO
      }
    }
  }
  object xml {
    object etree {
      object ElementTree {
        abstract class Element extends Iterable[Element] {
          def text: String = TODO
          def attrib: b.Dict[String, String] = TODO
          def findall(expr: String): Seq[Element]
        }

        def fromstring(s: String): Element = TODO
      }
    }
  }
}
