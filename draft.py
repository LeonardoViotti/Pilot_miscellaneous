
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
driver = webdriver.Ie()

# Get TFP website
driver.get(site)


# Click on link
#tf = driver.find_element_by_class_name('urLnkTxtStd')


# These elements go from ...Tfp_No_editor.0 to ...Tfp_No_editor.X
tf = driver.find_element_by_xpath("//A[@id='PKNP.SummaryView.Tfp_No_editor.2']/SPAN")
tf.click()

# Click "TF Details" tab
#tab = driver.find_element_by_link_text('https://p7p.worldbank.org/webdynpro/resources/worldbank.org/tfp_tabs_adm/Components/worldbank.org.TFP_Tabs_ADM/Tab_TF_Details.png')
#tab.click()