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

class TestExplorerGoesToElement():
  def setup_method(self, method):
    options = webdriver.FirefoxOptions()
    options.add_argument("--headless")
    self.driver = webdriver.Firefox(options=options)
    self.driver.implicitly_wait(10)
    self.wait = WebDriverWait(self.driver, timeout=10)
    self.vars = {}

  def teardown_method(self, method):
    self.driver.quit()

  def test_explorerGoesToElement(self):
    self.driver.get("http://localhost:8080/explore?q=%22tam%22")
    self.driver.find_element(By.CSS_SELECTOR, ".clickable:nth-child(2) > td:nth-child(3)").click()
    self.wait.until(EC.text_to_be_present_in_element((By.CSS_SELECTOR, ".title:nth-child(2)"), "Tam Lin Thrice"))
    self.wait.until(EC.title_is("Tam Lin Thrice | Set | Dancelor"))
