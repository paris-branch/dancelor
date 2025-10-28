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

import utils

class TestExplorerGoesToElement():
  def setup_method(self, method):
    utils.default_setup(self)

  def teardown_method(self, method):
    utils.default_teardown(self)

  def test_explorerGoesToElement(self):
    self.driver.get("http://localhost:8080/explore?q=%22tam%22")
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Tam Lin Thrice')]").click()
    self.wait.until(EC.text_to_be_present_in_element((By.XPATH, "//h2"), "Tam Lin Thrice"))
    self.wait.until(EC.title_is("Tam Lin Thrice | Set | Dancelor"))
