### --------------------------------------------------------------------------------------------
#
#  Script to read in GESLA tide gauges and save specific locations.
#
#   last edited: August 31, 2020 to include Altantic City
#
### --------------------------------------------------------------------------------------------

# LOAD LIBS ---------------------------------------------------------------
library(HURDAT) # USED TO PULL LATEST HURDAT TO TRY TO REMOVE THE EFFECTS OF HURRICANES ON TIDES
library(dplyr)
library(tidyverse)
library(mgcv)
library(tibble)
library(purrr)
library(extRemes)
library(leaflet)
library(chron)
library(MASS)
library(SWMPr)
library(oce)

## set the working directory
setwd("~/Google Drive/github_repos/slr/")

## set up directory for data files -- this is specific to CH
data_dir <- "~/Google Drive/github_repos/slr/data/"

# read in relevant tide gauge stations metadata
tide_gauge_meta <- readr::read_csv(paste0(data_dir,"tide_gauge_meta_clean.csv"))

# pointer to directory for GESLA tide gauge data -- allows us to get to extremes etc.
#gesla_dir <- "gesla/public_14032017"
local_data_dir <- "~/Documents/risQ/data/"
gesla_dir <- "~/Documents/risQ/sea_level_rise/gesla/"
output_dir <- "~/Documents/risQ/WaterQuality/data/"

# get total us boundary coordinates so we can fit a simple spline model to the whole east coast US boundary
us_boundary_lat_long <- sf::read_sf(paste0(local_data_dir,"cb_2017_us_nation_20m/cb_2017_us_nation_20m.shp")) %>%
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::select(long = X, lat = Y) %>%
  dplyr::filter(lat <= max(tide_gauge_meta$lat) & 
                  lat >= min(tide_gauge_meta$lat) &
                  long <= max(tide_gauge_meta$long) & 
                  long >= min(tide_gauge_meta$long))


# PULL HURICANE DAT DATA 
# hurdat <- HURDAT::get_hurdat(basin = "AL") %>%
#   mutate(category = case_when(Wind >= 137 ~ "5",
#                               Wind < 137 & Wind >= 113 ~ "4",
#                               Wind < 113 & Wind >= 96 ~ "3",
#                               Wind < 96 & Wind >= 83 ~ "2",
#                               Wind < 83 & Wind >= 64 ~ "1",
#                               Wind < 64 & Wind >= 34 ~ "tropical_storm",
#                               Wind < 34 ~ "tropical_depression"),
#          Year = lubridate::year(DateTime))


# FUNCTIONS FOR READING AND PROCESSING ONE GAUGE FROM GESLA, REMOVING HURRICANES --------------
# ---------------------------------------------------------------------------------------------
# take out outliers from time series --------
outlier_remove <- function(gauge_data){
  gauge_data <- gauge_data %>%
    dplyr::filter(qc_flag == 1) 
  
  outlier_ind <- which(gauge_data$sea_level %>%
                         scale() < -5) # seems to work, but should check some
  if(length(outlier_ind) > 0){
    gauge_data <- gauge_data[-outlier_ind,]
  }
  return(gauge_data)
}

# read in the tide gauge data, compute long-term trend, then detrend  --------
read_gauge <- function(fname = "gesla/public_14032017/fortpulaski-8670870-usa-noaa"){
  wl_dat <- read.table(file = fname, 
                       skip = 32, header=FALSE) %>%
    as_tibble() %>%
    rename(date = V1, time = V2, sea_level = V3, qc_flag = V4, extreme_flag = V5) %>%
    mutate(date = as.character(date)) %>%
    mutate(year = stringr::str_sub(date, 1, 4), month = stringr::str_sub(date, 6, 7), day = stringr::str_sub(date, 9, 10)) %>%
    dplyr::mutate(datetimestamp = as.POSIXct(lubridate::ymd(date) + lubridate::hms(time))) 
  
  station <- strsplit(fname, "/")[[1]][length(strsplit(fname, "/")[[1]])]
  wl_dat$gesla_fname <- station
  
  wl_dat <- left_join(wl_dat, 
                      tide_gauge_meta %>%
                        dplyr::select(gesla_fname, lat, long) %>%
                        dplyr::distinct(), 
                      by=c("gesla_fname")) %>%
    # converts sea level to inches
    dplyr::mutate(sea_level = sea_level*39.3701) %>%
    # remove outliers before detrending the data
    outlier_remove()
  
  # add column with detrended sl value
  df = data.frame(tt=wl_dat$datetimestamp,sea_level=wl_dat$sea_level)
  wl_dat$sl_detrend <- wl_dat$sea_level - predict(rlm(sea_level ~ tt, df,maxit=120))
    
  return(wl_dat)
}

# join the gauge data with the hurricane database --------
hurdat_join <- function(gauge_dat){
  # match all tide gauge extremes dates (year-month-day) with a hurricane (if they coincide), 
  #    compute distance b/w gauge & hurricane
  dat_hurdat_join <- left_join(gauge_dat %>% 
                                 dplyr::mutate(date=lubridate::date(date)), 
                               hurdat %>% 
                                 # get lat lon to join by distance
                                 dplyr::select(Key, Name, DateTime, category, 
                                               lat_hurr = Lat, lon_hurr = Lon) %>%
                                 dplyr::filter(category %in% c(1,2,3,4,5)) %>%
                                 dplyr::mutate(date = lubridate::date(DateTime)) %>%
                                 dplyr::select(-DateTime) %>%
                                 dplyr::distinct(), 
                               by=c("date")) %>%
    dplyr::mutate(euclidean_distance = (sqrt((long - lon_hurr)^2) + sqrt((lat - lat_hurr)^2)))
  dat_hurdat_join
}

# removed days that coincide with a hurricane close by --------
#  note:  nor'easters will not be removed b/c not in the hurdat database
filter_hurricane_impact <- function(gauge_dat){
  gauge_dat %>%
    dplyr::filter(euclidean_distance > 5 | is.na(euclidean_distance)) %>%
    dplyr::select( -lat_hurr, -lon_hurr, -euclidean_distance ) %>%
    dplyr::distinct()
}

# ---------------------------------------------------------------------------------------------

# extract the filenames
gesla_fnames <- tide_gauge_meta$gesla_fname %>% 
  unique()

# read in the data and join with the hurricane dataset 
gauge_list <- purrr::map(.x = gesla_fnames, 
                         .f = ~ read_gauge(fname = file.path(gesla_dir, .x))) 
names(gauge_list) <- gesla_fnames

## extract only the gauges of interest
## 'capemay', 'fortpulaski','springmaidpier','charlestoni'

gauge_subset <- list(CapeMay = gauge_list$`capemay-8536110-usa-noaa`,
                     ReedyPoint = gauge_list$`reedypoint-8551910-usa-noaa`,
                     AtlanticCity = gauge_list$`atlanticcity-8534720-usa-noaa`,
                     FortPulaski = gauge_list$`fortpulaski-8670870-usa-noaa`,
                     SpringmaidPier = gauge_list$`springmaidpier-8661070-usa-noaa`,
                     CharlestonI = gauge_list$`charlestoni-8665530-usa-noaa`)


saveRDS(gauge_subset,paste0(output_dir,'TideGauge_DailyData.rds'))
  

