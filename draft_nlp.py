
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
raw = file.read()
# Good practice to close the file to avoid curruption
file.close()

#--------------------------------------------------------------------
# Processing

# Not sure if this is a good idea since a lot of these are tables, but
# it is the easiest way to start
tokens = word_tokenize(raw)


# Remove parenthesis
list(filter(lambda s: s not in [")", '('], tokens))

#--------------------------------------------------------------------
# Foo


