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

class TestQuickSearchToComplexSearch():
  def setup_method(self, method):
    options = webdriver.FirefoxOptions()
    options.headless = True
    self.driver = webdriver.Firefox(options=options)
    self.vars = {}

  def teardown_method(self, method):
    self.driver.quit()

  def test_quickSearchToComplexSearch(self):
    self.driver.implicitly_wait(1)
    self.driver.get("http://localhost:8080/")
    self.driver.find_element(By.CSS_SELECTOR, "input:nth-child(2)").send_keys("tam lin")
    self.driver.find_element(By.CSS_SELECTOR, "input:nth-child(2)").send_keys(Keys.ENTER)
    assert self.driver.find_element(By.CSS_SELECTOR, "h2").text == "Search"
    # assert self.driver.find_element(By.CSS_SELECTOR, "input:nth-child(2)").get_attribute("value") == "tam lin"
