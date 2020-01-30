
#--------------------------------------------------------------------
# TFP scraping draft
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Setttings

import pandas as pd
from selenium import webdriver

site = 'https://p7p.worldbank.org/webdynpro/dispatcher/worldbank.org/tfp_summary/TFP_Summary'

#--------------------------------------------------------------------
# Driver

driver = webdriver.Ie()
driver.get(site)
