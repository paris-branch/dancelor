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

class TestSearchGoesToElement():
  def setup_method(self, method):
    options = webdriver.FirefoxOptions()
    options.headless = True
    self.driver = webdriver.Firefox(options=options)
    self.driver.implicitly_wait(1)
    self.vars = {}

  def teardown_method(self, method):
    self.driver.quit()

  def test_searchGoesToElement(self):
    self.driver.get("http://localhost:8080/search?q=%22tam%22")
    self.driver.find_element(By.CSS_SELECTOR, ".clickable:nth-child(2) > td:nth-child(3)").click()
    assert self.driver.find_element(By.CSS_SELECTOR, ".title:nth-child(1)").text == "Tam Lin Thrice"
    assert self.driver.title == "Tam Lin Thrice | Set | Dancelor"
