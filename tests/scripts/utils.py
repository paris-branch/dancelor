from selenium import webdriver
from selenium.webdriver.support.wait import WebDriverWait

def default_setup(self):
    options = webdriver.FirefoxOptions()
    options.add_argument("--headless")
    self.driver = webdriver.Firefox(options=options)
    self.driver.set_window_size(1080, 4320)
    self.driver.implicitly_wait(10)
    self.wait = WebDriverWait(self.driver, timeout=10)
    self.vars = {}

def default_teardown(self):
    self.driver.quit()
