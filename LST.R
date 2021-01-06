##install.packages("sp")
##install.packages("raster")
##install.packages("rgeos")
##install.packages("rgdal")
##install.packages("ggplot2")
##install.packages("stringr")
##install.packages("raster")
##install.packages("fs")
##install.packages("sf")
##install.packages("tidyverse")
##install.packages("rasterVis")
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(rasterVis)
library(ggplot2)
library(stringr)
library(raster)
library(fs)
library(sf)
library(tidyverse)
library(here)
library(magrittr)
library(RStoolbox)
library(raster)

manc<-dir_info(here::here("data"))%>%
  dplyr::filter(str_detect(path, "[B1234567].TIF")) %>%
  dplyr::filter(str_detect(path, "B11", negate=TRUE))%>%
  dplyr::select(path)%>%
  pull()%>%
  stack()

names(manc) <- c('ultra-blue', 'blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2') 

NDVIfun <- function(NIR, Red) {
  NDVI <- (NIR - Red) / (NIR + Red)
  return(NDVI)
}

ndvi <- NDVIfun(manc$NIR, manc$red)

ndvi %>%
  plot(.,col = rev(terrain.colors(10)), main = "Landsat-NDVI")

ndvi %>%
  hist(., breaks = 40, main = "NDVI Histogram", xlim = c(-.3,.8))

MTL<-dir_info(here::here("data")) %>%
  dplyr::filter(str_detect(path, "MTL.txt")) %>%
  dplyr::select(path)%>%
  pull()%>%
  readMeta()

head(MTL)

offsetandgain <-MTL %>%
  getMeta("B10_dn", metaData = ., what = "CALRAD")

offsetandgain

b10<-dir_info(here::here("data"))%>%
  dplyr::filter(str_detect(path, "B10"))%>%
  dplyr::select(path)%>%
  pull()%>%
  raster()

TOA <- offsetandgain$gain * b10 + offsetandgain$offset

Calidata <- MTL$CALBT%>%
  as.data.frame()%>%
  mutate(Band=rownames(.))%>%
  filter(Band=="B10_dn")

# subset the columns
K1 <- Calidata %>%
  dplyr::select(K1)%>%
  pull()

K2 <- Calidata %>%
  dplyr::select(K2)%>%
  pull()

Brighttemp <- (K2 / log((K1 / TOA) + 1))

facveg <- (ndvi-0.2/0.5-0.2)^2
emiss <- 0.004*facveg+0.986
Boltzmann <- 1.38e-23
Plank <- 6.626e-34
c <- 2.998e8

p <- Plank*(c/Boltzmann)

lambda <- 1.09e-5

LST <- Brighttemp/(1 +(lambda*Brighttemp/p)*log(emiss))

LST <- LST-273.15
plot(LST)

writeRaster(LST, "F:\\OneDrive - University College London\\CASA_2020\\GIS\\Final\\work\\T.tif", format='GTiff',overwrite=TRUE)
