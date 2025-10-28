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

class TestQuickSearch():
  def setup_method(self, method):
    utils.default_setup(self)

  def teardown_method(self, method):
    utils.default_teardown(self)

  def test_default(self):
    self.driver.get("http://localhost:8080/")
    ## Write “tam lin” in the input, then click on the “Tam Lin Thrice” element
    ## on the page.
    self.driver.find_element(By.XPATH, "//button[text()[contains(., 'Search')]]").click()
    self.driver.find_element(By.XPATH, "//input").send_keys("tam lin")
    self.driver.find_element(By.XPATH, "//*[contains(text(), \'Tam Lin Thrice\')]").click()
    ## and check the result
    self.wait.until(EC.text_to_be_present_in_element((By.XPATH, "//h2"), "Tam Lin Thrice"))
    self.wait.until(EC.title_is("Tam Lin Thrice | Set | Dancelor"))

  def test_fromOtherPage(self):
    ## Same as `default` but from other page.
    self.driver.get("http://localhost:8080/tune/tam-lin")
    ## Write “tam lin” in the input, then click on the “Tam Lin Thrice” element
    ## on the page.
    self.driver.find_element(By.XPATH, "//button[text()[contains(., 'Search')]]").click()
    self.driver.find_element(By.XPATH, "//input").send_keys("tam lin")
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Tam Lin Thrice')]").click()
    ## and check the result
    self.wait.until(EC.text_to_be_present_in_element((By.XPATH, "//h2"), "Tam Lin Thrice"))
    self.wait.until(EC.title_is("Tam Lin Thrice | Set | Dancelor"))

  # def test_keys(self):
  #   ## Same as `default` but use keys and Enter to navigate the results.
  #   self.driver.get("http://localhost:8080/")
  #   ## Write “tam lin” in the input, then press the down arrow thrice, then
  #   ## press Enter.
  #   self.driver.find_element(By.XPATH, "//input").send_keys("tam lin")
  #   time.sleep(1)
  #   self.driver.find_element(By.XPATH, "//input").send_keys(Keys.DOWN)
  #   self.driver.find_element(By.XPATH, "//input").send_keys(Keys.DOWN)
  #   self.driver.find_element(By.XPATH, "//input").send_keys(Keys.DOWN)
  #   self.driver.find_element(By.XPATH, "//input").send_keys(Keys.ENTER)
  #   ## Check the result
  #   self.wait.until(EC.text_to_be_present_in_element((By.XPATH, "//h2[@class='title']"), "Tam Lin Thrice"))
  #   self.wait.until(EC.title_is("Tam Lin Thrice | Set | Dancelor"))

  def test_slashToFocus(self):
    ## Same as `default` but use slash to focus.
    self.driver.get("http://localhost:8080/")
    ## Press '/', then enter "tam lin" in the focused element and click on the
    ## “Tam Lin Thrice” element on the page.
    self.driver.find_element(By.CSS_SELECTOR, "body").send_keys('/')
    time.sleep(2) ## FIXME: there is an ugly timer in Dancelor, so there is one in the test as well
    self.driver.switch_to.active_element.send_keys("tam lin")
    self.driver.find_element(By.XPATH, "//*[contains(text(), 'Tam Lin Thrice')]").click()
    ## Check the result
    self.wait.until(EC.text_to_be_present_in_element((By.XPATH, "//h2"), "Tam Lin Thrice"))
    self.wait.until(EC.title_is("Tam Lin Thrice | Set | Dancelor"))

  def test_buttonToExplorer(self):
    ## Same as `default` but press Enter to go to the explorer.
    self.driver.get("http://localhost:8080/")
    self.driver.find_element(By.XPATH, "//button[text()[contains(., 'Search')]]").click()
    self.driver.find_element(By.XPATH, "//input").send_keys("tam lin")
    self.driver.find_element(By.XPATH, "//div[@class = 'modal-footer']//button[text()[contains(., 'Explore')]]").click()
    self.wait.until(EC.text_to_be_present_in_element((By.XPATH, "//h2"), "Explore"))
    # self.wait.until(EC.text_to_be_present_in_element_value((By.XPATH, "//input"), "tam lin"))

  def test_enterToExplorer(self):
    ## Same as `default` but press Enter to go to the explorer.
    self.driver.get("http://localhost:8080/")
    self.driver.find_element(By.XPATH, "//button[text()[contains(., 'Search')]]").click()
    self.driver.find_element(By.XPATH, "//input").send_keys("tam lin")
    self.driver.find_element(By.XPATH, "//input").send_keys(Keys.ENTER)
    self.wait.until(EC.text_to_be_present_in_element((By.XPATH, "//h2"), "Explore"))
    # self.wait.until(EC.text_to_be_present_in_element_value((By.XPATH, "//input"), "tam lin"))
