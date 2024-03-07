import pytest
import time
import json
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities

class TestContextLinks():
  def setup_method(self, method):
    options = webdriver.FirefoxOptions()
    options.add_argument("--headless")
    self.driver = webdriver.Firefox(options=options)
    self.driver.implicitly_wait(1)
    self.wait = WebDriverWait(self.driver, timeout=1)
    self.vars = {}

  def teardown_method(self, method):
    self.driver.quit()

  def test_fromExplorerSearch(self):
    ## From the explorer, type “tam”, then click on the set “Tam Lin Thrice” and
    ## check that the resulting URL contains the right context.
    self.driver.get("http://localhost:8080/explore")
    self.driver.find_element(By.CSS_SELECTOR, "input:nth-child(2)").send_keys("tam")
    time.sleep(1)
    self.driver.find_element(By.CSS_SELECTOR, ".clickable:nth-child(2) > td:nth-child(3)").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/set/tam-lin-thrice?context=%5B%22InSearch%22%2C%22tam%22%5D"))

  def test_fromExplorerURI(self):
    ## From the explorer loaded with query “tam”, click on the set “Tam Lin
    ## Thrice” and check that the resulting URL contains the right context.
    self.driver.get("http://localhost:8080/explore?q=%22tam%22")
    self.driver.find_element(By.CSS_SELECTOR, ".clickable:nth-child(2) > td:nth-child(3)").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/set/tam-lin-thrice?context=%5B%22InSearch%22%2C%22tam%22%5D"))

  def test_sideArrowGoesToNeighbour(self):
    ## From the set “Tam Lin Thrice” in the context of a search for “tam”, check
    ## that clicking on the arrow to the left goes to “The Tam Lin Book”.
    self.driver.get("http://localhost:8080/set/tam-lin-thrice?context=%5B%22InSearch%22%2C%22tam%22%5D")
    self.driver.find_element(By.CSS_SELECTOR, ".context-links-left .context-links-main").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/book/the-tam-lin-book?context=%5B%22InSearch%22%2C%22tam%22%5D"));

  def test_keyGoesToNeighbour(self):
    ## From the set “Tam Lin Thrice” in the context of a search for “tam”, check
    ## that pressing the left key goes to “The Tam Lin Book”.
    self.driver.get("http://localhost:8080/set/tam-lin-thrice?context=%5B%22InSearch%22%2C%22tam%22%5D")
    self.driver.find_element(By.CSS_SELECTOR, "body").send_keys(Keys.LEFT)
    self.wait.until(EC.url_to_be("http://localhost:8080/book/the-tam-lin-book?context=%5B%22InSearch%22%2C%22tam%22%5D"));

  def test_bannerUpwardsTriangleGoesToContext(self):
    ## From the set “Tam Lin Thrice” in the context of a search for “tam”, check
    ## that clicking the banner's upwards triangle goes back to the explorer.
    self.driver.get("http://localhost:8080/set/tam-lin-thrice?context=%5B%22InSearch%22%2C%22tam%22%5D")
    self.driver.find_element(By.LINK_TEXT, "▴").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/explore?q=%22tam%22"))

  def test_bannerCrossRemovesContext(self):
    ## From the set “Tam Lin Thrice” in the context of a search for “tam”, check
    ## that clicking the banner's cross removes the context.
    self.driver.get("http://localhost:8080/set/tam-lin-thrice?context=%5B%22InSearch%22%2C%22tam%22%5D")
    self.driver.find_element(By.LINK_TEXT, "⨉").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/set/tam-lin-thrice"))
