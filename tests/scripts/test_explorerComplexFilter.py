from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC

import utils

class TestExplorerComplexFilter():
  def setup_method(self, method):
    utils.default_setup(self)

  def teardown_method(self, method):
    utils.default_teardown(self)

  def test_explorerComplexFilter(self):
    self.driver.get("http://localhost:8080/explore?q=%22tam%22")
    utils.wait_and_click(self.driver, By.XPATH, "//button[text()[contains(., 'Filter')]]")
    utils.wait_and_click(self.driver, By.XPATH, "//label[contains(text(), 'Book')]")
    utils.wait_and_click(self.driver, By.XPATH, "//button[text()[contains(., 'Apply')]]")
    utils.wait_for_element(self.driver, By.XPATH, "//div[contains(text(), 'Showing 1 to 1 of 1 entries')]")
    utils.wait_and_click(self.driver, By.XPATH, "//button[text()[contains(., 'Filter')]]")
    utils.wait_and_click(self.driver, By.XPATH, "//label[contains(text(), 'Set')]")
    utils.wait_and_click(self.driver, By.XPATH, "//label[contains(text(), 'Polka')]")
    utils.wait_and_click(self.driver, By.XPATH, "//label[contains(text(), 'Waltz')]")
    utils.wait_and_click(self.driver, By.XPATH, "//button[text()[contains(., 'Apply')]]")
    utils.wait_for_element(self.driver, By.XPATH, "//*[contains(text(), 'Your search returned no result')]")
    self.wait.until(EC.text_to_be_present_in_element((By.CSS_SELECTOR, ".alert-warning"), "Your search returned no results."))
    input_element = utils.wait_for_element(self.driver, By.XPATH, "//div[@class = 'container-md']//input")
    assert input_element.get_attribute("value") == "type:Set kind:version:base:(Polka :or Waltz) tam "
