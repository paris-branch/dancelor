import os
import pytest
import time
import json
import html
import tempfile
import shutil
import pyperclip
import yaml
from urllib.parse import urlparse

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities

import utils

class TestActions():
  def setup_method(self, method):
    utils.default_setup(self)

  def teardown_method(self, method):
    utils.default_teardown(self)

  def get_downloaded_file(self):
    files = [f for f in os.listdir(self.download_dir) if os.path.isfile(os.path.join(self.download_dir, f))]
    if len(files) == 0: raise FileNotFoundError("No files found in the download directory")
    elif len(files) > 1: raise ValueError(f"Multiple files found in the download directory: {files}")
    return os.path.join(self.download_dir, files[0])

  def test_versionShowLilyPond(self):
    self.driver.get("http://localhost:8080/version/xzzb-wasm-babe")
    self.driver.find_element(By.XPATH, "(//i[contains(@class, 'bi-three-dots-vertical')])[2]").click()
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Show LilyPond')]").click()
    with open("tests/database/version/xzzb-wasm-babe/meta.yaml") as meta_file:
      [kind, payload] = yaml.safe_load(meta_file)["content"]
      assert (kind == "Monolithic")
      expected = payload["lilypond"]
    shown = self.driver.find_element(By.XPATH, "//pre[contains(text(), 'clef')]").get_attribute("innerHTML")
    assert(html.unescape(shown.strip()) == expected.strip())
    ## TODO: also check the “copy to clipboard” functionality
    # self.driver.find_element(By.XPATH, "//*[contains(text(), 'Copy to clipboard')]").click()
    # time.sleep(1)  # Wait for clipboard to update
    # copied = pyperclip.paste()
    # assert(copied == expected)
