#------------------------------------------------------------------------------#

#				Pavuna - RAIS anslysis DRAFT

#------------------------------------------------------------------------------#  


#------------------------------------------------------------------------------#
#### Paths ####

DROPBOX <- "C:/Users/WB519128/Dropbox/Work/WB/RJ - Pavuna"
GITHUB <- "C:/Users/WB519128/Documents/GitHub/Pilot_miscellaneous"

DATA <- file.path(DROPBOX, "Data")
RAIS <- file.path(DATA, "RAIS/RAW")


#------------------------------------------------------------------------------#
#### Packages ####

library(data.table)

#------------------------------------------------------------------------------#
#### Load data ####

rais <- fread(file.path(RAIS,"RJ2016.csv"), header = T)
#rais <- read.csv(file.path(RAIS,"RJ2016.csv"), header = T)



#------------------------------------------------------------------------------#
#### Key objects ####

bairros_pavuna <- c("1160", # Pavuna
                    "200",  # Costa Barros
                    "90",   # Barros Filho
                    "40",   # Acari
                    "230")  # Coelho Neto


#------------------------------------------------------------------------------#
#### Key objects ####