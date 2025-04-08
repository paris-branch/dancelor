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

class TestVersionLyDownload():
  def setup_method(self, method):
    options = webdriver.FirefoxOptions()
    options.add_argument("--headless")
    self.driver = webdriver.Firefox(options=options)
    self.driver.implicitly_wait(10)
    self.vars = {}

  def teardown_method(self, method):
    self.driver.quit()

  def test_versionLyDownload(self):
    self.driver.get("http://localhost:8080/version/tam-lin-niols")
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Actions')]").click()
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'LilyPond')]").click()
    ## NOTE: Check that the URL is what we expect.
    assert(urlparse(self.driver.current_url).path == "/api/version/tam-lin-niols.ly")
    ## NOTE: Check that the `content.ly` is indeed shown on the page. We cannot
    ## check an equality because the page source might contain some HTML tags;
    ## it might also come out as escaped HTML so we need to check that too.
    with open("tests/database/version/tam-lin-niols/content.ly") as content_file:
      expected_content = content_file.read().strip()
      assert(expected_content != "")
    assert((expected_content in self.driver.page_source)
           or (expected_content in html.unescape(self.driver.page_source)))
