
#--------------------------------------------------------------------
# Settings

import nltk
from nltk import word_tokenize

import os
import csv
import numpy as np

# from bs4 import BeautifulSoup

#--------------------------------------------------------------------
# Load file

os.chdir("C:/Users/wb519128/Desktop")

file = open("tf_rf.txt", "r") 
rf = [line.split(' ') for line in file.readlines()] # Turn into List
# raw = file.read()
# Good practice to close the file to avoid curruption
file.close()

# tokens = word_tokenize(raw)

#--------------------------------------------------------------------
# Processing




# Flaten the list
rf = [j for sub in rf for j in sub]

# Remove line breaks and white space stuff
rf = [s.strip('\n') for s in rf]
rf = [s.strip('\x0c') for s in rf]
rf = [s.strip('(') for s in rf]
rf = [s.strip(')') for s in rf]

# Remove white space - not sure if this is a good idea, but is the simplest
# way I can think of processing this for now
rf = list(filter(lambda s: s != '', rf))
