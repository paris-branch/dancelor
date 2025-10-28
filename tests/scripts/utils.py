import tempfile
import shutil
from selenium import webdriver
from selenium.webdriver.support.wait import WebDriverWait

def default_setup(self):
    self.firefox_profile_path = tempfile.mkdtemp(prefix = "dancelor-selenium-firefox-profile-")
    firefox_options = webdriver.FirefoxOptions()
    firefox_options.profile = webdriver.FirefoxProfile(self.firefox_profile_path)
    firefox_options.add_argument("--headless")
    self.driver = webdriver.Firefox(options = firefox_options)
    self.driver.set_window_size(1080, 4320)
    self.driver.implicitly_wait(10)
    self.wait = WebDriverWait(self.driver, timeout = 10)
    self.vars = {}

def default_teardown(self):
    shutil.rmtree(self.firefox_profile_path, ignore_errors = True)
    self.driver.quit()
