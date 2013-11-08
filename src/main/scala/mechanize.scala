import scala.collection.mutable

import com.madmode.py2scala.batteries._

object mechanize {
  class Browser {
    import com.madmode.py2scala.__builtin__._
    def set_handle_robots(x: Boolean): Unit = TODO
    
    var method: String = TODO
    var addheaders: mutable.IndexedSeq[(String, String)] = TODO
    
    def open(fullurl: String): urllib.addinfourl
    def open(fullurl: String, data: String): urllib.addinfourl
  }
}