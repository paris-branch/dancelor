import pytest
import time
import json
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities

## This test is not broken in itself and it runs fine in Selenium IDE. However,
## running it within the Nix environment yields:
##
## FileNotFoundError: [Errno 2] No such file or directory: '/nix/store/<hash>-python3-3.10.5-env/lib/python3.10/site-packages/selenium/webdriver/remote/getAttribute.js'
##
## so we disable it for now by prefixing it with `Broken`.

class BrokenTestSetPDFDownload():
  def setup_method(self, method):
    options = webdriver.FirefoxOptions()
    options.add_argument("--headless")
    self.driver = webdriver.Firefox(options=options)
    self.driver.implicitly_wait(1)
    self.vars = {}

  def teardown_method(self, method):
    self.driver.quit()

  def test_setPDFDownload(self):
    self.driver.get("http://localhost:8080/set/tam-lin-thrice")
    self.driver.find_element(By.LINK_TEXT, "PDF").click()
    link = self.driver.find_element(By.LINK_TEXT, "Download").get_attribute("href")
    assert(link == "/api/set//tam-lin-thrice.pdf")
    self.driver.find_element(By.CSS_SELECTOR, "label:nth-child(6)").click()
    self.driver.find_element(By.CSS_SELECTOR, "tr:nth-child(2) label:nth-child(4)").click()
    link = self.driver.find_element(By.LINK_TEXT, "Download").get_attribute("href")
    assert(link == "/api/set//tam-lin-thrice.pdf?parameters=%7B%22every-version%22:%7B%22transposition%22:%5B%22Relative%22%2C%22Eb%22%2C%22C%2C%22%5D%2C%22instruments%22:%22E%E2%99%AD%20instruments%22%2C%22clef%22:%22bass%22%7D%7D")
