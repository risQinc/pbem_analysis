##################################################################################################################
##                                          read_usgs_data.R
##
##  Read in the file from USGS with peak stream flow data downloaded from:
##    https://nwis.waterdata.usgs.gov/nwis/peak?search_criteria=obs_date_range&submitted_form=introduction 
##
## Date: 08-18-2020
## Author: CH
##
##  - Reads un file downloaded from usgs which seems to have peak discharge rates from ~29,000 guages
##  - modified so that CI is no longer calculated along with return period discharges 
##      - when minimum number of years was decreased to 5, some sites were problematic but don't have flags
##    
##
##################################################################################################################

library(sf)
library(sp)
library(lubridate)
library(gridExtra)
library(MASS)
library(mgcv)
library(Metrics)
library(tictoc)
library(stats)
library(tidyverse)
library(ggplot2)
library(forecast)
library(tictoc)
library(stringr)
library(mapdata)
library(mapview)
library(leaflet)
library(dplyr)

wd <- "~/Documents/risQ/Inland_Flooding/"
DATA_DIR <- paste0(wd,"Data/")
OUTPUT_DIR <- paste0(wd,"Output/")

##################################################################################################################
##   Read in the text file containing the annual maximum discharge
##################################################################################################################

# This information includes the following fields:
#
#  agency_cd     Agency Code
#  site_no       USGS station number
#  peak_dt       Date of peak streamflow (format YYYY-MM-DD)
#  peak_tm       Time of peak streamflow (24 hour format, 00:00 - 23:59)
#  peak_va       Annual peak streamflow value in cfs
#  peak_cd       Peak Discharge-Qualification codes (see explanation below)
#  gage_ht       Gage height for the associated peak streamflow in feet
#  gage_ht_cd    Gage height qualification codes
#  year_last_pk  Peak streamflow reported is the highest since this year
#  ag_dt         Date of maximum gage-height for water year (if not concurrent with peak)
#  ag_tm         Time of maximum gage-height for water year (if not concurrent with peak
#  ag_gage_ht    maximum Gage height for water year in feet (if not concurrent with peak
#  ag_gage_ht_cd maximum Gage height code
#
# Peak Streamflow-Qualification Codes(peak_cd):
#   1 ... Discharge is a Maximum Daily Average
#   2 ... Discharge is an Estimate
#   3 ... Discharge affected by Dam Failure
#   4 ... Discharge less than indicated value,
#           which is Minimum Recordable Discharge at this site
#   5 ... Discharge affected to unknown degree by
#           Regulation or Diversion
#   6 ... Discharge affected by Regulation or Diversion
#   7 ... Discharge is an Historic Peak
#   8 ... Discharge actually greater than indicated value
#   9 ... Discharge due to Snowmelt, Hurricane,
#           Ice-Jam or Debris Dam breakup
#   A ... Year of occurrence is unknown or not exact
#   Bd ... Day of occurrence is unknown or not exact
#   Bm ... Month of occurrence is unknown or not exact
#   C ... All or part of the record affected by Urbanization,
#            Mining, Agricultural changes, Channelization, or other
#   F ... Peak supplied by another agency
#   O ... Opportunistic value not from systematic data collection
#   R ... Revised
#
# Gage height qualification codes(gage_ht_cd,ag_gage_ht_cd):
#   1 ... Gage height affected by backwater
#   2 ... Gage height not the maximum for the year
#   3 ... Gage height at different site and(or) datum
#   4 ... Gage height below minimum recordable elevation
#   5 ... Gage height is an estimate
#   6 ... Gage datum changed during this year
#   7 ... Debris, mud, or hyper-concentrated flow
#   8 ... Gage height tidally affected
#   Bd ... Day of occurrence is unknown or not exact
#   Bm ... Month of occurrence is unknown or not exact
#   F ... Peak supplied by another agency
#   R ... Revised

amax <-  read_delim(paste0(DATA_DIR,"peak_data.txt"), delim = "\t", skip = 29457, col_names = FALSE)  
colnames(amax) <- c('agency_code','site_no','peak_dt','peak_tm','peak_va','peak_cd','gage_ht','gage_ht_cd','year_last_pk',
                    'ag_dt','ag_tm','ag_gage_ht','ag_gage_ht_cd')

