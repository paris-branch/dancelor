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

class TestQuickSearchToComplexSearch():
  def setup_method(self, method):
    options = webdriver.FirefoxOptions()
    options.add_argument("--headless")
    self.driver = webdriver.Firefox(options=options)
    self.driver.implicitly_wait(1)
    self.wait = WebDriverWait(self.driver, timeout=1)
    self.vars = {}

  def teardown_method(self, method):
    self.driver.quit()

  def test_quickSearchToComplexSearch(self):
    self.driver.get("http://localhost:8080/")
    self.driver.find_element(By.CSS_SELECTOR, "input:nth-child(2)").send_keys("tam lin")
    self.driver.find_element(By.CSS_SELECTOR, "input:nth-child(2)").send_keys(Keys.ENTER)
    self.wait.until(EC.text_to_be_present_in_element((By.CSS_SELECTOR, "h2"), "Explore"))
    # self.wait.until(EC.text_to_be_present_in_element_value((By.CSS_SELECTOR, "input:nth-child(2)"), "tam lin"))
