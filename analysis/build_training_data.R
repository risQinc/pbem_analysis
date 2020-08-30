library(data.table)
library(lubridate)
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
library(mde)


tide_data <- readRDS('../data/raw/TideGauge_DailyData.rds')
tide_data = as.data.table(tide_data)

#Time Range 1: November 1, 1964 to November 1, 1966
#Time Range 2: November 1997 to November 1999)

relevant_tide_dat = tide_data[(CapeMay.datetimestamp <= '1999-11-01' & CapeMay.datetimestamp >= '1997-11-01' ) | 
                                (CapeMay.datetimestamp >= '1964-11-01' & CapeMay.datetimestamp <= '1966-11-01')]

# cape may sealevel
# fort pulasaki sealevel
# springmaid pier sealevel
# charleston
relevant_tide_dat$CapeMay.date = as.Date(relevant_tide_dat$CapeMay.date)

# which locations do we need? 
# is sea_level the right field to be using? we just want to take the average by date right? 
# is it ok that we are missing 1964 data?

######################################################
############# GAUGE DATA
######################################################


# seems only to have discharge values ?
sr <-  read_delim("../data/raw/Meyer_Schuylkill_River.txt", delim = "\t", skip = 50, col_names = FALSE)  
colnames(sr) <- c('agency_cd', 'site_no',	'date',  'avg_discharge_cfps', 'na1', 'max_sc_mc_sie_p_cm_25c', 'na2', 'min_sc_mc_sie_p_cm_25c', 'na3', 'avg_sc_mc_sie_p_cm_25c', 'na4', 'max_dis_o2', 'na5', 'min_dis_o2', 'na6', 'avg_dis_o2', 'na7', 'max_temp_c2', 'na8', 'min_temp_c2', 'na9', 'avg_temp_c2', 'na10', 'max_ph', 'na11', 'min_ph', 'na12', 'avg_sus_sed_conc', 'na13', 'avg_sus_sed_discharge', 'na14', 'max_temp_f', 'na15', 'min_temp_f', 'na16', 'avg_temp_f', 'na17', 'avg_temp_c', 'na18', 'max_temp_c', 'na19', 'min_temp_c', 'na20')

missingness_sr = percent_missing(sr)
# only first 5 columns have have complete data
sr_clean = sr[, c('agency_cd', 'site_no',	'date',  'avg_discharge_cfps', 'na1')]
head(sr_clean)


# seems only to have min SC values ?
dr = read_delim("../data/raw/Meyer_Delaware_River_Trenton.txt", delim = "\t", skip = 68, col_names = FALSE)  
missingness_dr = percent_missing(dr)

# columsn 1,2,3, 28 29
colnames(dr) <- c('agency_cd', 'site_no',	'date','missing_4', 'missing_5', 'missing_6', 'missing_7', 'missing_8', 'missing_9', 'missing_10', 'missing_11', 'missing_12', 'missing_13', 'missing_14', 'missing_15', 'missing_16', 'missing_17', 'missing_18', 'missing_19', 'missing_20', 'missing_21', 'missing_22', 'missing_23', 'missing_24', 'missing_25', 'missing_26', 'missing_27', 'sc_code', 'max_sc_25c', 'missing_30', 'missing_31', 'missing_32', 'missing_33', 'missing_34', 'missing_35', 'missing_36', 'missing_37', 'missing_38', 'missing_39', 'missing_40', 'missing_41', 'missing_42', 'missing_43', 'missing_44', 'missing_45', 'missing_46', 'missing_47', 'missing_48', 'missing_49', 'missing_50', 'missing_51', 'missing_52', 'missing_53', 'missing_54', 'missing_55', 'missing_56', 'missing_57', 'missing_58', 'missing_59', 'missing_60', 'missing_61', 'missing_62', 'missing_63', 'missing_64', 'missing_65', 'missing_66', 'missing_67', 'missing_68', 'missing_69', 'missing_70', 'missing_71', 'missing_72', 'missing_73', 'missing_74', 'missing_75')

dr_clean = dr[, c('agency_cd', 'site_no',	'date', 'sc_code', 'max_sc_25c')]
head(dr_clean)


