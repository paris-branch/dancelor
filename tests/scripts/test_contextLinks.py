import time
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC

import utils

class TestContextLinks():
  def setup_method(self, method):
    utils.default_setup(self)

  def teardown_method(self, method):
    utils.default_teardown(self)

  def test_fromExplorerSearch(self):
    ## From the explorer, type “tam”, then click on the set “Tam Lin Thrice” and
    ## check that the resulting URL contains the right context.
    self.driver.get("http://localhost:8080/explore")
    self.driver.find_element(By.XPATH, "//div[@class = 'container-md']//input").send_keys("tam")
    time.sleep(1)
    self.driver.find_element(By.XPATH, "//a[contains(text(), 'Tam Lin Thrice')]").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/set/ului-yd9x-o35w?context=%5B%22In_search%22%2C%22tam%22%5D"))

  def test_fromExplorerURI(self):
    ## From the explorer loaded with query “tam”, click on the set “Tam Lin
    ## Thrice” and check that the resulting URL contains the right context.
    self.driver.get("http://localhost:8080/explore?q=%22tam%22")
    self.driver.find_element(By.XPATH, "//a[contains(text(), 'Tam Lin Thrice')]").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/set/ului-yd9x-o35w?context=%5B%22In_search%22%2C%22tam%22%5D"))

  def test_fromSet(self):
    ## From the set “Tam Lin Thrice”, click on the second “Tam Lin” version and
    ## check that the resulting URL contains the right context.
    self.driver.get("http://localhost:8080/set/ului-yd9x-o35w")
    self.driver.find_element(By.XPATH, "(//a[text() = 'Tam Lin'])[2]").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/tune/qdod-ad7l-8gr2/xzzb-wasm-babe?context=%5B%22In_set%22%2C%22ului-yd9x-o35w%22%2C1%5D"))

  def test_fromBook(self):
    ## From the book “The Tam Lin Book”, click on the set “Tam Lin Thrice” and
    ## check that the resulting URL contains the right context.
    self.driver.get("http://localhost:8080/book/0fi3-1iot-6tbq")
    time.sleep(1) # give a second to avoid clicking on placeholder
    self.driver.find_element(By.XPATH, "//tr[2]/td[2]//a[not(ancestor::small)]").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/set/ului-yd9x-o35w?context=%5B%22In_book%22%2C%220fi3-1iot-6tbq%22%2C1%5D"))

  def test_sideArrowGoesToNeighbour(self):
    ## From the set “Tam Lin Thrice” in the context of a search for “tam”, check
    ## that clicking on the arrow to the left goes to “The Tam Lin Book”.
    self.driver.get("http://localhost:8080/set/ului-yd9x-o35w?context=%5B%22In_search%22%2C%22tam%22%5D")
    self.driver.find_element(By.XPATH, "//a[contains(@class, 'btn')]//i[contains(@class, 'bi-arrow-left')]").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/tune/qdod-ad7l-8gr2/xzzb-wasm-babe?context=%5B%22In_search%22%2C%22tam%22%5D"))

  def test_bannerUndoGoesToContext(self):
    ## From the set “Tam Lin Thrice” in the context of a search for “tam”, check
    ## that clicking the banner's “undo” icon goes back to the explorer.
    self.driver.get("http://localhost:8080/set/ului-yd9x-o35w?context=%5B%22In_search%22%2C%22tam%22%5D")
    self.driver.find_element(By.XPATH, "//a[contains(@class, 'btn')]//i[contains(@class, 'bi-arrow-counterclockwise')]").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/explore?q=%22tam%22"))

  def test_bannerCloseRemovesContext(self):
    ## From the set “Tam Lin Thrice” in the context of a search for “tam”, check
    ## that clicking the banner's “close” icon removes the context.
    self.driver.get("http://localhost:8080/set/ului-yd9x-o35w?context=%5B%22In_search%22%2C%22tam%22%5D")
    self.driver.find_element(By.XPATH, "//a[contains(@class, 'btn')]//i[contains(@class, 'bi-eraser')]").click()
    self.wait.until(EC.url_to_be("http://localhost:8080/set/ului-yd9x-o35w"))
