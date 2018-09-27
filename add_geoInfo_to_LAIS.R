#------------------------------------------------------------------------------#

#	  LVTP - Cadastre maps around corridor

#------------------------------------------------------------------------------#


CAD_temp <- file.path("C:/Users/Leonardo/Dropbox/Work/cadaster/districts")
OUT_temp <- file.path("C:/Users/Leonardo/Dropbox/Work/WB/Rwanda Feeder Roads/data/admin/LAIS/LAIS data request 2018/Combined")

#------------------------------------------------------------------------------#
#### Load data ####

#### Load LAIS
lais_E <- read.csv(file.path(LAIS2018, "WB_Eastern.csv"), header = T, sep = ";", stringsAsFactors=FALSE)
lais_W <- read.csv(file.path(LAIS2018, "WB_Western.csv"), header = T, sep = ";", stringsAsFactors=FALSE)
lais_S <- read.csv(file.path(LAIS2018, "WB_Southern.csv"), header = T, sep = ";", stringsAsFactors=FALSE)
lais_N <- read.csv(file.path(LAIS2018, "WB_Northern.csv"), header = T, sep = ";", stringsAsFactors=FALSE)



#------------------------------------------------------------------------------#
#### PROCESS LAIS ####

#### Change factor levels with crazy characteds

lais_E$description <- gsub("Â","", lais_E$description)
lais_W$description <- gsub("Â","", lais_W$description)
lais_S$description <- gsub("Â","", lais_S$description)
lais_N$description <- gsub("Â","", lais_N$description)

#### Combine datasets ####
lais <- rbind(lais_E, lais_W, lais_S, lais_N)

# Fix upi
lais$upi <-as.numeric(gsub("/", "", lais$upi))

#------------------------------------------------------------------------------#
#### Only districts around corridor ####
cor_dist <- c("Bugesera",
              "Ngoma",
              "Nyanza",
              "Ruhango",
              "Kamonyi",
              "Huye",
              #"Gisagara",
              "Rwamagana",
              "Kayonza",
              "Kirehe")

#------------------------------------------------------------------------------#
#### Load Cadaster ####

cor_dist <- tolower(cor_dist)

start_time <- Sys.time()


for(dis in cor_dist){
#for(dis in "huye"){

  
  cad_i <- NULL
  cad_i <- readOGR(file.path(CAD_temp, dis),
                   dis)

  #cad_i <- spTransform(cad_i, RwaProj)

  cad_i_data <- NULL
  cad_i_data <- cad_i@data
  
  assign(paste0("data_", dis),
         cad_i_data)

  rm(cad_i)
}

end_time <- Sys.time()
end_time - start_time


data_list <- paste0("data_", cor_dist)

# Create dataset
cadData <- rbind(data_bugesera,
                 data_ngoma,
                 data_nyanza,
                 data_ruhango,
                 data_kamonyi,
                 data_huye, 
                 data_rwamagana,
                 data_kayonza,
                 data_kirehe)

#------------------------------------------------------------------------------#
#### Merge with LAIS ####

names(lais) <- c("description", 
                 "UPI",           
                 "area",
                 "approval_date", 
                 "fees_amount",
                 "village_name" )


lais_exp <- merge(lais, cadData, by = "UPI")


write.csv(lais_exp,
          file= file.path(OUT_temp, "LAIS18.csv"),
          row.names = F)
