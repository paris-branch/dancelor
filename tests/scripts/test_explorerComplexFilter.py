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

class TestExplorerComplexFilter():
  def setup_method(self, method):
    options = webdriver.FirefoxOptions()
    options.add_argument("--headless")
    self.driver = webdriver.Firefox(options=options)
    self.driver.implicitly_wait(10)
    self.wait = WebDriverWait(self.driver, timeout=10)
    self.vars = {}

  def teardown_method(self, method):
    self.driver.quit()

  def test_explorerComplexFilter(self):
    self.driver.get("http://localhost:8080/explore?q=%22tam%22")
    self.driver.find_element(By.XPATH, "//button[text()[contains(., 'Filter')]]").click()
    self.driver.find_element(By.XPATH, "//label[contains(text(), 'Book')]").click()
    self.driver.find_element(By.XPATH, "//button[text()[contains(., 'Apply')]]").click()
    self.driver.find_element(By.XPATH, "//div[contains(text(), 'Showing 1 to 1 of 1 entries')]")
    self.driver.find_element(By.XPATH, "//button[text()[contains(., 'Filter')]]").click()
    self.driver.find_element(By.XPATH, "//label[contains(text(), 'Set')]").click()
    self.driver.find_element(By.XPATH, "//label[contains(text(), 'Polka')]").click()
    self.driver.find_element(By.XPATH, "//label[contains(text(), 'Waltz')]").click()
    self.driver.find_element(By.XPATH, "//button[text()[contains(., 'Apply')]]").click()
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Your search returned no result')]")
    self.wait.until(EC.text_to_be_present_in_element((By.CSS_SELECTOR, ".alert-warning"), "Your search returned no results."))
    assert self.driver.find_element(By.XPATH, "//div[@class = 'container']//input").get_attribute("value") \
      == "type:Set kind:version:base:(Polka :or Waltz) tam"
