###############################################################################################################################
##                                           get_sl_proj.R
##
## date created: 07-21-2020
## date last modifies: 07-21-2020
##
## Description of Script:
##
## 1) Extracts SL projections at specific locations
##
################################################################################################################################

## set the working directory
setwd("~/Google Drive/github_repos/WaterQuality/")

## set up directory for data files
data_dir <- "~/Google Drive/github_repos/slr/data/"
K2017_dir <- "~/Google Drive/github_repos/slr/Kopp_2017/"
output_dir <- "~/Documents/risQ/WaterQuality/data/"

## load the necessary libraries
#library(sf)
library(tidyverse)
library(purrr)
#library(mgcv) # key lib that has the modeling functions
#library(rmapshaper)
library(dplyr)
library(rgeos)
#library(mapview)
#library(leaflet)
#library(Metrics)
#library(stats)

## list of locations we are interested in 
# Delaware River
# Savannah River
# Pee Dee/Wakana (sp?) River

# read in file containing subset of tide gauges
daily_data <- readRDS(paste0(output_dir,"TideGauge_DailyData.rds"))
city_names <- c('CAPE MAY','REEDY POINT','ATLANTIC CITY','FORT PULASKI','SPRINGMAID PIER','CHARLESTON I')
data_names <- names(daily_data)

tide_gauge_meta <- readr::read_csv(paste0(data_dir,"tide_gauge_meta_clean.csv"))

# read in Kopp (2017) background rates
bkgd_rates <- read.csv(paste0(K2017_dir,'eft2271-sup-0003-2017ef000663-ds02.tsv'), sep = "\t")

# DS3: Kopp (2014) projections
k14 <- read.csv(paste0(K2017_dir,'eft2271-sup-0004-2017ef000663-ds03.tsv'), skip=1, sep = "\t")

# DS4: Deconto and Pollard (2016) projections with bias correction
dp16 <- read.csv(paste0(K2017_dir,'eft2271-sup-0005-2017ef000663-ds04.tsv'), skip=1, sep = "\t")

# find corresponding ID's between GESLA and PSMSL database by finding minimum distance location 
# note: not all locations correspong --> set an additional criteria?
condensed_proj_k14 = NULL
condensed_proj_dp16 = NULL

for (ii in seq(1:length(city_names))){
 
 gesla_name <- daily_data[[data_names[ii]]]$gesla_fname[1]
  
  ## extract K14
  rcp45 <- filter(k14,Site.Name==city_names[ii], Scenario=="rcp45", Year<=2100) %>%
    dplyr::select(Site.Name,Site.ID,Scenario,Year,X0.05,X0.5,X0.95) 
  rcp85 <- filter(k14,Site.Name==city_names[ii], Scenario=="rcp85", Year<=2100) %>%
    dplyr::select(Site.Name,Site.ID,Scenario,Year,X0.05,X0.5,X0.95)
  
  condensed_proj_k14 = rbind(condensed_proj_k14, 
                             inner_join(rcp45 %>% 
                                          dplyr::rename(rcp45_Q0.05 = X0.05,
                                                        rcp45_Q0.5 = X0.5,
                                                        rcp45_Q0.95 = X0.95) %>%
                                          dplyr::select(-Scenario),rcp85 %>% 
                                          dplyr::rename(rcp85_Q0.05 = X0.05,
                                                        rcp85_Q0.5 = X0.5,
                                                        rcp85_Q0.95 = X0.95) %>%
                                          dplyr::select(-Scenario), by = c("Site.Name", "Site.ID", "Year") 
                             ) %>% #convert to inches
                               mutate(rcp45_Q0.05 = 0.393701*rcp45_Q0.05,
                                      rcp45_Q0.5 = 0.393701*rcp45_Q0.5,
                                      rcp45_Q0.95 = 0.393701*rcp45_Q0.95,
                                      rcp85_Q0.05 = 0.393701*rcp85_Q0.05,
                                      rcp85_Q0.5 = 0.393701*rcp85_Q0.5,
                                      rcp85_Q0.95 = 0.393701*rcp85_Q0.95,
                                      lat = tide_gauge_meta$lat[tide_gauge_meta$gesla_fname==gesla_name],
                                      long = tide_gauge_meta$long[tide_gauge_meta$gesla_fname==gesla_name])
  ) 
  
  ## extract DP14
  rcp45 <- filter(dp16,Site.Name==city_names[ii], Scenario=="rcp45", Year<=2100) %>%
    dplyr::select(Site.Name,Site.ID,Scenario,Year,X0.05,X0.5,X0.95) 
  rcp85 <- filter(dp16,Site.Name==city_names[ii], Scenario=="rcp85", Year<=2100) %>%
    dplyr::select(Site.Name,Site.ID,Scenario,Year,X0.05,X0.5,X0.95)
  
  condensed_proj_dp16 = rbind(condensed_proj_dp16, 
                             inner_join(rcp45 %>% 
                                          dplyr::rename(rcp45_Q0.05 = X0.05,
                                                        rcp45_Q0.5 = X0.5,
                                                        rcp45_Q0.95 = X0.95) %>%
                                          dplyr::select(-Scenario),rcp85 %>% 
                                          dplyr::rename(rcp85_Q0.05 = X0.05,
                                                        rcp85_Q0.5 = X0.5,
                                                        rcp85_Q0.95 = X0.95) %>%
                                          dplyr::select(-Scenario), by = c("Site.Name", "Site.ID", "Year") 
                             ) %>% #convert to inches
                               mutate(rcp45_Q0.05 = 0.393701*rcp45_Q0.05,
                                      rcp45_Q0.5 = 0.393701*rcp45_Q0.5,
                                      rcp45_Q0.95 = 0.393701*rcp45_Q0.95,
                                      rcp85_Q0.05 = 0.393701*rcp85_Q0.05,
                                      rcp85_Q0.5 = 0.393701*rcp85_Q0.5,
                                      rcp85_Q0.95 = 0.393701*rcp85_Q0.95,
                                      lat = tide_gauge_meta$lat[tide_gauge_meta$gesla_fname==gesla_name],
                                      long = tide_gauge_meta$long[tide_gauge_meta$gesla_fname==gesla_name])
  ) 
  
}

## save the outpout
sl_projections <- list(condensed_proj_k14=condensed_proj_k14, condensed_proj_dp16=condensed_proj_dp16)
fileout <- paste0(output_dir,"condensed_sl_proj.rds")
saveRDS(sl_projections,fileout)


### plot one of the projections
tmp <- filter(condensed_proj_k14,Site.Name=="BOSTON")
ggplot() +
  # plot all simulations
  geom_line(data = tmp, aes(x = Year, y = rcp45_Q0.5), color = "black", alpha = 0.8, size = 0.65) +
  geom_line(data = tmp, aes(x = Year, y = rcp45_Q0.95), color = "black", alpha = 0.2, size = 0.65) +
  geom_line(data = tmp, aes(x = Year, y = rcp45_Q0.05), color = "black", alpha = 0.2, size = 0.65) +
  
  geom_line(data = tmp, aes(x = Year, y = rcp85_Q0.5), color = "red", alpha = 0.8, size = 0.65) +
  geom_line(data = tmp, aes(x = Year, y = rcp85_Q0.95), color = "red", alpha = 0.2, size = 0.65) +
  geom_line(data = tmp, aes(x = Year, y = rcp85_Q0.05), color = "red", alpha = 0.2, size = 0.65) +
  
  labs(x="Year",y="Sea Level (inches)") +
  theme_bw()  +
  theme(text = element_text(size=20))






