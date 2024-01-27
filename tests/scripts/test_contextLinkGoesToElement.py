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

class TestContextLinkGoesToElement():
  def setup_method(self, method):
    options = webdriver.FirefoxOptions()
    options.headless = True
    self.driver = webdriver.Firefox(options=options)
    self.driver.implicitly_wait(1)
    self.vars = {}

  def teardown_method(self, method):
    self.driver.quit()

  def test_contextLinkGoesToElementWithClick(self):
    self.driver.get("http://localhost:8080/set/tam-lin-thrice?in-search=%22tam%22")
    self.driver.find_element(By.CSS_SELECTOR, ".context-link-left .context-link-main").click()
    assert self.driver.find_element(By.CSS_SELECTOR, ".title:nth-child(1)").text == "The Tam Lin Book"
    assert self.driver.title == "The Tam Lin Book | Book | Dancelor"

  def test_contextLinkGoesToElementWithArrow(self):
    self.driver.get("http://localhost:8080/set/tam-lin-thrice?in-search=%22tam%22")
    self.driver.find_element(By.CSS_SELECTOR, "body").send_keys(Keys.LEFT)
    assert self.driver.find_element(By.CSS_SELECTOR, ".title:nth-child(1)").text == "The Tam Lin Book"
    assert self.driver.title == "The Tam Lin Book | Book | Dancelor"
