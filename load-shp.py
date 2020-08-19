import pandas as pd
import geopandas as gpd
import geoplot
import matplotlib.pyplot as plt


path = "C:/Users/wb519128/WBG/Sveta Milusheva - COVID 19 Results/Uganda/geofiles/raw/"

a2 = gpd.read_file(path  + 'uga_admbnda_adm2_2020.shp')
a3 = gpd.read_file(path  + 'uga_admbnda_adm3_2020.shp')
a2 = gpd.read_file(path  + 'uga_admbnda_adm2_2020.shp')

geoplot.polyplot(a3)
plt.show()