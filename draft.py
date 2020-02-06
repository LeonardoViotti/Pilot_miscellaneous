
#--------------------------------------------------------------------
# TFP scraping draft
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Setttings

import pandas as pd
from selenium import webdriver

# Define TFP page
site = 'https://p7p.worldbank.org/webdynpro/dispatcher/worldbank.org/tfp_summary/TFP_Summary'

#--------------------------------------------------------------------
# Driver

# Open IE
# WARNING: Need to select "Enable Protected Mode" for all zones in Tools> Internet Options > Security

driver = webdriver.Ie()

# Get TFP website
driver.get(site)

# Click on TF number
# These elements go from ...Tfp_No_editor.0 to ...Tfp_No_editor.X
tf = driver.find_element_by_xpath("//A[@id='PKNP.SummaryView.Tfp_No_editor.2']/SPAN")
tf.click()

# Click "TF Details" tab
details_tab = driver.find_element_by_xpath("//A[@id='PKNPMJJN.TabsView.TF_Details_Tab_LTA']")
details_tab.click()


