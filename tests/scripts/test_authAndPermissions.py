import pytest
import time
import json
import html
from urllib.parse import urlparse

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities

class TestAuthAndPermissions():
  def setup_method(self, method):
    options = webdriver.FirefoxOptions()
    options.add_argument("--headless")
    self.driver = webdriver.Firefox(options=options)
    self.driver.implicitly_wait(10)
    self.vars = {}

  def teardown_method(self, method):
    self.driver.quit()

  def test_auth_and_permissions(self):
    self.driver.get("http://localhost:8080/person/a-private-person")
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Oooops')]")
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Sign in')]").click()
    self.driver.find_element(By.XPATH, "//input[@placeholder = 'jeanmilligan']").send_keys("niols")
    self.driver.find_element(By.XPATH, "//input[@placeholder = '1234567']").send_keys("test")
    self.driver.find_element(By.XPATH, "//button[text()[contains(., 'Sign in')] and not(contains(@class, 'disabled'))]").click()
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'A Private Person')]")
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Niols')]").click()
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Sign out')]").click()
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Oooops')]")
