package selenium.webdriver.remote.webdriver

import com.madmode.py2scala.__builtin__.TODO
import selenium.webdriver.remote.webelement

/**
 * Created by connolly on 10/18/13.
 */
abstract class WebDriver {
  def get(addr: String)
  def find_element_by_xpath(xp: String): webelement.WebElement
  def find_element_by_link_text(text: String): webelement.WebElement
}
