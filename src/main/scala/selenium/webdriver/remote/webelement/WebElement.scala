package selenium.webdriver.remote.webelement

/**
 * Created by connolly on 10/18/13.
 */
trait WebElement {
  def find_element_by_name(n: String): WebElement
  def find_element_by_xpath(xp: String): WebElement
  def click()
  def clear()
  def send_keys(s: String)
}
