package com.madmode.pfmorris

object getpath {
  import __builtin__._

  import batteries.os

  def getpath(filename: String): String = {
    if (os.path.isfile(filename)) {
      return filename
    }
    if (os.getenv("TEXPATH")) {
      val another_name = os.path.join(os.getenv("TEXPATH"), filename)
      if (os.path.isfile(another_name)) {
        return another_name
      }
    }
    var home_dir: String = null
    if (os.name == "posix") {
      home_dir = os.getenv("HOME")
    } else {
      home_dir = '.'
    }
    val config_file_name = os.path.join(home_dir, ".proofcheck")
    if (os.path.isfile(config_file_name)) {
      val config_file = open(config_file_name)
      var next_line = config_file.readline().strip()
      while (next_line && next_line.startswith('#')) {
        next_line = config_file.readline().strip()
      }
      config_file.close()
      val user_proofcheck_directory_name = next_line
      if (!os.path.isdir(user_proofcheck_directory_name)) {
        println(user_proofcheck_directory_name + "not a directory!")
        throw new SystemExit()
      }
      val second_try_name = os.path.join(user_proofcheck_directory_name, filename)
      if (os.path.isfile(second_try_name)) {
        return second_try_name
      }
    }
    val pipe = os.popen(("kpsewhich " + filename))
    val kpathtry_name = pipe.read().strip()
    if (kpathtry_name) {
      return kpathtry_name
    }
    return null
  }
  def main(args: Array[String]): Unit = {
    val filename = raw_input("Enter file name: ")
    println(getpath(filename))
  }
}
