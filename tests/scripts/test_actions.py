import os
import pytest
import time
import json
import html
import tempfile
import shutil
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
    self.download_dir = tempfile.mkdtemp()
    options.set_preference("browser.download.folderList", 2)
    options.set_preference("browser.download.dir", self.download_dir)
    options.set_preference("browser.download.useDownloadDir", True)
    options.set_preference("browser.helperApps.neverAsk.saveToDisk", "")
    self.driver = webdriver.Firefox(options=options)
    self.driver.implicitly_wait(10)
    self.vars = {}

  def teardown_method(self, method):
    shutil.rmtree(self.download_dir)
    self.driver.quit()

  def get_downloaded_file(self):
    files = [f for f in os.listdir(self.download_dir) if os.path.isfile(os.path.join(self.download_dir, f))]
    if len(files) == 0: raise FileNotFoundError("No files found in the download directory")
    elif len(files) > 1: raise ValueError(f"Multiple files found in the download directory: {files}")
    return os.path.join(self.download_dir, files[0])

  def test_versionLyDownload(self):
    self.driver.get("http://localhost:8080/version/xzzb-wasm-babe")
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Actions')]").click()
    lilypond_button = self.driver.find_element(By.XPATH, "//*[contains(text(), 'LilyPond')]")
    ## NOTE: Check that the URL is what we expect.
    assert(urlparse(lilypond_button.get_attribute('href')).path == "/api/version/xzzb-wasm-babe/tam-lin.ly")
    lilypond_button.click()
    ## NOTE: Check that the `content.ly` is indeed what we downloaded.
    with open("tests/database/version/xzzb-wasm-babe/content.ly") as content_file:
      expected_content = content_file.read().strip()
      assert(expected_content != "")
    with open(self.get_downloaded_file()) as content_file:
      actual_content = content_file.read().strip()
      assert(actual_content != "")
    assert(actual_content == expected_content)
