package selenium.webdriver.chrome.webdriver

import com.madmode.py2scala.__builtin__._
import selenium.webdriver.remote
import remote.webelement.WebElement
import selenium.webdriver.chrome.options

/**
 * Created by connolly on 10/18/13.
 */
class WebDriver(chrome_options: options.Options) extends remote.webdriver.WebDriver {
  def get(addr: String) = TODO

  def find_element_by_xpath(xp: String): WebElement = TODO

  def find_element_by_link_text(text: String): WebElement = TODO
}
