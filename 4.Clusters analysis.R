#This file is from a personal analysis I was using to teach myself GIS. Later, it turned into a project with PeaceTech Lab to analyze NFI needs in South Sudan
#A majority of the analysis was conducted in JMP and Tableau, but this code shows my experience with GIS in R

#Install and access
install.packages("tidyverse")
install.packages("ggmap")
install.packages("sf")
install.packages("sp")
install.packages("mapview")
install.packages("ggrepel")
install.packages("devtools")
install.packages("plyr")
library(plyr)
library(devtools)
library(ggrepel)
library(tidyverse)
library(readxl)
library(ggmap)
library(sf)
library(mapview)
devtools::install_github("r-spatial/mapview@develop")

#google key and read files
register_google(key="AIzaSyCt36rfLElJP0Hqd1jaRDNKfIf05_g62Iw")
chi_shape <- st_read("C:/Users/brody/Downloads/ssd_admbnda_adm1_imwg_nbs_20180817/ssd_admbnda_adm1_imwg_nbs_20180817.shp")
as_tibble(chi_shape)
S.Sudan.Data <- read_excel("C:/Users/brody/Desktop/School/R Code GIS/DTM_SSD_VNA_R6_new.xlsx")

#subset the data and clean it up a bit
S.Sudan.Data.Subset <- S.Sudan.Data[c(11,13,12,9,8,33,47,95,23,159,160,161,162,163,164,165,167,173,174)]
S.Sudan.Data.Subset[1,]
S.Sudan.Data <- read.csv("C:/Users/brody/Desktop/School/R Code GIS/DTM_SSD_VNA_R6.csv")
S.Sudan.Data.Subset$Total.Refugees <- S.Sudan.Data$X..of.returnees..individuals.+S.Sudan.Data$X..of.IDPs..individuals.

#####Overall view####
#Prep for mapview and create a map with background as state boundaries
Place <- st_as_sf(S.Sudan.Data.Subset, coords = c("Longitude", "Latitude"),crs = 4326)
x <- mapview(chi_shape, zcol = "ADM1_EN", legend=T) + mapview(Place, zcol="Cluster", col.regions = c('forestgreen', "springgreen", 'gold', "deepskyblue", 'darkorange', "red1", "deeppink1","blueviolet","green","khaki1"), legend=T, cex = "Total.Refugees")
x

#Save
mapshot(x, url = paste0(getwd(), "/Cluster view (DTM South Sudan Village Assessment and South Sudan administrative level 0-2 boundaries by OCHA).html"))

####Distance to Nearest Education Facility####
#subset the data and clean it up a bit
S.Sudan.Data.Subset <- S.Sudan.Data[c(11,13,12,9,8,95,23,159,160,161,162,163,164,165,166)]
S.Sudan.Data.Subset$Total.Refugees <- S.Sudan.Data$X..of.returnees..individuals.+S.Sudan.Data$X..of.IDPs..individuals.
S.Sudan.Data.Subset$Percentage.of.children.attending.primary.learning.facility <- mapvalues(S.Sudan.Data.Subset$Percentage.of.children.attending.primary.learning.facility, from= c("0%","25%"), to= c("<25%","<25%"))
S.Sudan.Data.Subset$Distance.of.education.facility <- factor(S.Sudan.Data.Subset$Distance.of.education.facility, levels = c("<1 km","1-2 km", "3-5 km", "6-10 km", ">10 km", "Unknown", NA))

#Prep for mapview and create a map with background as state boundaries
Place <- st_as_sf(S.Sudan.Data.Subset, coords = c("Longitude", "Latitude"),crs = 4326)
x <- mapview(chi_shape, zcol = "ADM1_EN", legend=T) + mapview(Place, zcol="Distance.of.education.facility", col.regions = c('forestgreen', "springgreen", 'gold', "gold", 'darkorange', "red1", "gray0"), legend=T, cex = "Total.Refugees")
x

#Save
mapshot(x, url = paste0(getwd(), "/Distance to Nearest Education Facility(DTM South Sudan Village Assessment and South Sudan administrative level 0-2 boundaries by OCHA).html"))

####Percentage of children Attending a primary learning facility####
#subset the data and clean it up a bit
S.Sudan.Data.Subset <- S.Sudan.Data[c(11,13,12,9,8,95,23,159,160,161,162,163,164,165,166)]
S.Sudan.Data.Subset$Total.Refugees <- S.Sudan.Data$X..of.returnees..individuals.+S.Sudan.Data$X..of.IDPs..individuals.
S.Sudan.Data.Subset$Percentage.of.children.attending.primary.learning.facility <- mapvalues(S.Sudan.Data.Subset$Percentage.of.children.attending.primary.learning.facility, from= c("0%","25%"), to= c("<25%","<25%"))
S.Sudan.Data.Subset$Percentage.of.children.attending.primary.learning.facility <- factor(S.Sudan.Data.Subset$Percentage.of.children.attending.primary.learning.facility, levels = c("<25%", "25-50%", "51-75%", ">75%", "Unknown", NA))

#Prep for mapview and create a map with background as state boundaries
Place <- st_as_sf(S.Sudan.Data.Subset, coords = c("Longitude", "Latitude"),crs = 4326)
x <- mapview(chi_shape, zcol = "ADM1_EN", legend=T) + mapview(Place, zcol="Percentage.of.children.attending.primary.learning.facility", col.regions = c("red1", 'gold','springgreen',"springgreen",'forestgreen',"darkorange"), legend=T, cex = "Total.Refugees")
x

#Save
mapshot(x, url = paste0(getwd(), "/Percentage of Children Attending Primary Learning Facilities(DTM South Sudan Village Assessment and South Sudan administrative level 0-2 boundaries by OCHA).html"))

####Types of education faciity####
#subset the data and clean it up a bit
S.Sudan.Data.Subset <- S.Sudan.Data[c(11,13,12,9,8,95,23,159,160,161,162,163,164,165,166)]
S.Sudan.Data.Subset$Total.Refugees <- S.Sudan.Data$X..of.returnees..individuals.+S.Sudan.Data$X..of.IDPs..individuals.
S.Sudan.Data.Subset$Percentage.of.children.attending.primary.learning.facility <- mapvalues(S.Sudan.Data.Subset$Percentage.of.children.attending.primary.learning.facility, from= c("0%","25%"), to= c("<25%","<25%"))

#Prep for mapview and create a map with background as state boundaries
Place <- st_as_sf(S.Sudan.Data.Subset, coords = c("Longitude", "Latitude"),crs = 4326)
x <- mapview(chi_shape, zcol = "ADM1_EN", legend=T) + mapview(Place, zcol="Type.of.education.facility", col.regions = c("forestgreen", 'forestgreen',"red1"), legend=T, cex = "Total.Refugees")
x

#Save
mapshot(x, url = paste0(getwd(), "/Type of Education Facility (DTM South Sudan Village Assessment and South Sudan administrative level 0-2 boundaries by OCHA).html"))
