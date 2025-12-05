from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys

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
    utils.wait_and_click(self.driver, By.XPATH, "//button[text()[contains(., 'Search')]]")
    utils.wait_and_send_keys(self.driver, By.XPATH, "//input", "tam lin")
    utils.wait_and_click(self.driver, By.XPATH, "//*[contains(text(), \'Tam Lin Thrice\')]")
    ## and check the result
    self.wait.until(EC.text_to_be_present_in_element((By.XPATH, "//h2"), "Tam Lin Thrice"))
    self.wait.until(EC.title_is("Tam Lin Thrice | Set | Dancelor"))

  def test_fromOtherPage(self):
    ## Same as `default` but from other page.
    self.driver.get("http://localhost:8080/tune/tam-lin")
    ## Write "tam lin" in the input, then click on the "Tam Lin Thrice" element
    ## on the page.
    utils.wait_and_click(self.driver, By.XPATH, "//button[text()[contains(., 'Search')]]")
    utils.wait_and_send_keys(self.driver, By.XPATH, "//input", "tam lin")
    utils.wait_and_click(self.driver, By.XPATH, "//*[contains(text(), 'Tam Lin Thrice')]")
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
    ## "Tam Lin Thrice" element on the page.
    body = utils.wait_for_element(self.driver, By.CSS_SELECTOR, "body")
    body.send_keys('/')
    # Wait for the input to be focused and ready (instead of arbitrary sleep)
    self.wait.until(lambda d: d.switch_to.active_element.tag_name == "input")
    active_input = self.driver.switch_to.active_element
    self.wait.until(lambda d: active_input.is_displayed() and active_input.is_enabled())
    active_input.send_keys("tam lin")
    utils.wait_and_click(self.driver, By.XPATH, "//*[contains(text(), 'Tam Lin Thrice')]")
    ## Check the result
    self.wait.until(EC.text_to_be_present_in_element((By.XPATH, "//h2"), "Tam Lin Thrice"))
    self.wait.until(EC.title_is("Tam Lin Thrice | Set | Dancelor"))

  def test_buttonToExplorer(self):
    ## Same as `default` but press Enter to go to the explorer.
    self.driver.get("http://localhost:8080/")
    utils.wait_and_click(self.driver, By.XPATH, "//button[text()[contains(., 'Search')]]")
    utils.wait_and_send_keys(self.driver, By.XPATH, "//input", "tam lin")
    utils.wait_and_click(self.driver, By.XPATH, "//div[@class = 'modal-footer']//button[text()[contains(., 'Explore')]]")
    self.wait.until(EC.text_to_be_present_in_element((By.XPATH, "//h2"), "Explore"))
    # self.wait.until(EC.text_to_be_present_in_element_value((By.XPATH, "//input"), "tam lin"))

  def test_enterToExplorer(self):
    ## Same as `default` but press Enter to go to the explorer.
    self.driver.get("http://localhost:8080/")
    utils.wait_and_click(self.driver, By.XPATH, "//button[text()[contains(., 'Search')]]")
    utils.wait_and_send_keys(self.driver, By.XPATH, "//input", "tam lin" + Keys.ENTER)
    self.wait.until(EC.text_to_be_present_in_element((By.XPATH, "//h2"), "Explore"))
    # self.wait.until(EC.text_to_be_present_in_element_value((By.XPATH, "//input"), "tam lin"))
