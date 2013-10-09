

/**
 * Created by connolly on 10/6/13.
 */
object selenium {
  import com.madmode.py2scala.__builtin__.TODO

  object webdriver {
    object support {
      import remote.webdriver.WebDriver
      import remote.webelement.WebElement

      object wait {
        class WebDriverWait(d: WebDriver, wait_time: Int, poll_period: Int=3) {
          def until[T <: AnyRef](pred: WebDriver => T): WebElement = TODO
        }
      }

      object ui{
        class Select(webelement: WebElement) {
          def select_by_index(index: Int) = TODO
        }
      }
    }

    object remote {
      object webdriver {
        abstract class WebDriver {
          def get(addr: String)
          def find_element_by_xpath(xp: String): webelement.WebElement
          def find_element_by_link_text(text: String): webelement.WebElement
        }
      }
      object webelement {
        trait WebElement {
          def find_element_by_name(n: String): WebElement
          def find_element_by_xpath(xp: String): WebElement
          def click()
          def clear()
          def send_keys(s: String)
        }
      }
    }

    object chrome {
      import remote.webelement.WebElement

      object options {
        case class Options(var binary_location: String = null)
      }
      object webdriver {
        class WebDriver(opts: options.Options) extends remote.webdriver.WebDriver {
          def get(addr: String) = TODO

          def find_element_by_xpath(xp: String): WebElement = TODO

          def find_element_by_link_text(text: String): WebElement = TODO
        }
      }
    }
    class Chrome(opts: chrome.options.Options) extends chrome.webdriver.WebDriver(opts)
  }
}
