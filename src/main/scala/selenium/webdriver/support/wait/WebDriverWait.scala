package selenium.webdriver.support.wait

import com.madmode.py2scala.__builtin__._

import selenium.webdriver.remote.webdriver.WebDriver
import selenium.webdriver.remote.webelement.WebElement

/**
 * Created by connolly on 10/18/13.
 */
class WebDriverWait(d: WebDriver, wait_time: Int, poll_period: Int=3) {
  def until[T <: AnyRef](pred: WebDriver => T): WebElement = TODO
}
