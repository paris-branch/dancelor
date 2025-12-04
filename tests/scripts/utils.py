import tempfile
import shutil
from selenium import webdriver
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By

def default_setup(self):
    self.firefox_profile_path = tempfile.mkdtemp(prefix = "dancelor-selenium-firefox-profile-")
    firefox_options = webdriver.FirefoxOptions()
    firefox_options.profile = webdriver.FirefoxProfile(self.firefox_profile_path)
    firefox_options.add_argument("--headless")
    self.driver = webdriver.Firefox(options = firefox_options)
    self.driver.set_window_size(1080, 4320)
    # Remove implicit waits - use explicit waits instead for better reliability
    self.wait = WebDriverWait(self.driver, timeout = 10)
    self.vars = {}

def wait_and_click(driver, by, value, timeout=10):
    """Wait for element to be clickable and then click it."""
    element = WebDriverWait(driver, timeout).until(
        EC.element_to_be_clickable((by, value))
    )
    element.click()
    return element

def wait_and_send_keys(driver, by, value, keys, timeout=10):
    """Wait for element to be present and interactable, then send keys."""
    element = WebDriverWait(driver, timeout).until(
        EC.presence_of_element_located((by, value))
    )
    # Ensure element is visible and enabled
    WebDriverWait(driver, timeout).until(
        lambda d: element.is_displayed() and element.is_enabled()
    )
    element.send_keys(keys)
    return element

def wait_for_element(driver, by, value, timeout=10):
    """Wait for element to be present."""
    return WebDriverWait(driver, timeout).until(
        EC.presence_of_element_located((by, value))
    )

def default_teardown(self):
    shutil.rmtree(self.firefox_profile_path, ignore_errors = True)
    self.driver.quit()
