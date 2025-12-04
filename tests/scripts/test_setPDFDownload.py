from selenium.webdriver.common.by import By

import utils

## This test is not broken in itself and it runs fine in Selenium IDE. However,
## running it within the Nix environment yields:
##
## FileNotFoundError: [Errno 2] No such file or directory: '/nix/store/<hash>-python3-3.10.5-env/lib/python3.10/site-packages/selenium/webdriver/remote/getAttribute.js'
##
## so we disable it for now by prefixing it with `Broken`.

class BrokenTestSetPDFDownload():
  def setup_method(self, method):
    utils.default_setup(self)

  def teardown_method(self, method):
    utils.default_teardown(self)

  def test_setPDFDownload(self):
    self.driver.get("http://localhost:8080/set/tam-lin-thrice")
    utils.wait_and_click(self.driver, By.LINK_TEXT, "PDF")
    link_element = utils.wait_for_element(self.driver, By.LINK_TEXT, "Download")
    link = link_element.get_attribute("href")
    assert(link == "/api/set//tam-lin-thrice.pdf")
    utils.wait_and_click(self.driver, By.CSS_SELECTOR, "label:nth-child(6)")
    utils.wait_and_click(self.driver, By.CSS_SELECTOR, "tr:nth-child(2) label:nth-child(4)")
    link_element = utils.wait_for_element(self.driver, By.LINK_TEXT, "Download")
    link = link_element.get_attribute("href")
    assert(link == "/api/set//tam-lin-thrice.pdf?parameters=%7B%22every-version%22:%7B%22transposition%22:%5B%22Relative%22%2C%22Eb%22%2C%22C%2C%22%5D%2C%22instruments%22:%22E%E2%99%AD%20instruments%22%2C%22clef%22:%22bass%22%7D%7D")
