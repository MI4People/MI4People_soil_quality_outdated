# Required Libraries
library(sp)
library(rworldmap)
library(rgdal)
library(terra)
library(ggplot2)
library(ggmap)


# Loading preprocessed worldwide data from by Hengl et al.
# https://gitlab.com/openlandmap/compiled-ess-point-data-sets/-/tree/master
target_worldwide <- readRDS("~/Documents/Projects/MI4People/Code/sol_chem.pnts_horizons.rds")

# Get dimension of the table
dim(target_worldwide)

# Constrain data to Africa
# first define a function that will find the corresponding continent based on
# coordinates
coords2continent = function(points){  
    countriesSP <- getMap(resolution='low')
    # converting points to a SpatialPoints object
    # setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    indices$continent   # returns the continent (6 continent model)
    #indices$REGION   # returns the continent (7 continent model)
    #indices$ADMIN  #returns country name
    #indices$ISO3 # returns the ISO3 code 
}

# Get only the coordinates from target_worldwide
xy <- data.frame(lon=target_worldwide$longitude_decimal_degrees,
                 lat=target_worldwide$latitude_decimal_degrees)

# Apply coords2continent to these coordinates
continents <- coords2continent(xy)

# Add continents to original data
target_worldwide$continent <- continents

# Choose only data from Africa
target_Africa <- target_worldwide[target_worldwide$continent == 'Africa',]

# Check from which sources the data is coming. One observes that there are more
# relevant data sets than just AfSIS1 and AfSPDB (as was assumed during the very 
# first examination of data from Hengl et al.)
unique(target_Africa$source_db)

# Some statistic on the results
cat("Number of measurement points:", dim(target_Africa)[1])
cat("Number of unique locations:", length(unique(target_Africa$olc_id)))

# Statistics for pH-Value, organic carbon and total nitrogen that will 
# be the first target variables that we will consider
non_na_oc <-
    target_Africa[!is.na(target_Africa$oc), c('olc_id', 'oc')]
cat("Number of measurement points for organic carbon:", dim(non_na_oc)[1])
cat("Number of unique locations for organic carbon:",
    length(unique(non_na_oc$olc_id)))

non_na_n_tot <-
    target_Africa[!is.na(target_Africa$n_tot), c('olc_id', 'n_tot')]
cat("Number of measurement points for total nitrogen:", dim(non_na_n_tot)[1])
cat("Number of unique locations for total nitrogen:",
    length(unique(non_na_n_tot$olc_id)))

non_na_ph_h2o <-
    target_Africa[!is.na(target_Africa$ph_h2o), c('olc_id', 'ph_h2o')]
cat("Number of measurement points for pH-value:", dim(non_na_ph_h2o)[1])
cat("Number of unique locations for pH-value:",
    length(unique(non_na_ph_h2o$olc_id)))


# We focus on data for pH-Value, organic carbon and total nitrogen. So,
# we keep only those data samples where these measurements are available

clean_target <- target_Africa[(!is.na(target_Africa$oc)|
                                   !is.na(target_Africa$n_tot)|
                                   !is.na(target_Africa$ph_h2o)),]

dim(clean_target)


# Check all data scources contributing to clean_target. Again it is more
# than just AfSIS1 and AfSPDB
unique(clean_target$source_db)

# Now, plot locations of all target data on a map (it might take 1-2 minutes)
qmplot(longitude_decimal_degrees, latitude_decimal_degrees, data = clean_target,
       colour = I('red'), size = I(1))


# Now, let find out, how the data is destributed over different African
# countries
# First define a function that will find the corresponding country based on
# coordinates
coords2country = function(points){  
    countriesSP <- getMap(resolution='low')
    # converting points to a SpatialPoints object
    # setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    #indices$continent   # returns the continent (6 continent model)
    #indices$REGION   # returns the continent (7 continent model)
    indices$ADMIN  #returns country name
    #indices$ISO3 # returns the ISO3 code 
}

# Get only the coordinates from clean_target
xy <- data.frame(lon=clean_target$longitude_decimal_degrees,
                 lat=clean_target$latitude_decimal_degrees)

# Apply coords2country to these coordinates
countries <- coords2country(xy)
summary(countries)
# It looks like (almost) every African country is represented. However, some of
# them have only very few measurements. On the top are United Republic of
# Tanzania, Nigeria, and Angola with 5.000+ measurements each.


# To keep data clean, we get rid of continent column in clean_target since
# we do not need it anymore. We also drop several other columns that will
# (probably) be irrelevant for further analysis. In fact, we keep only 14
# columns.

# General columns:
# - uuid: a unique row ID
# - olc_id: unique location ID
# - site_obsdate: measurment date. Not always avaialable, sometimes date
#   (in different formats), sometime only the year. It must be made consistent
#   during the later analysis. How exactly, Data Scientists should decide:
#   probably we will take only the year
# - source_db: source of the data

# Coordinates columns:
# - longitude_decimal_degrees: longitude
# - latitude_decimal_degrees: latitude

# Horizons and depth of measurrements 
# - layer_sequence.f: if several measurements are done at one and the same
#   location but at5 different depths, this attribute notice the ordering of
#   depth. 1 means a measurement that has the lowest depth, 2 = measurement that
#   is made at greater depth as 1, 3 = even greater depth etc. 
# - hzn_top: upper boundary of horizon
# - hzn_bot: lower boundary of horizon
# - hzn_depth: is equal to hzn_top + (hzn_bot-hzn_top)/2 (depth of the
#   middle of the horizon)

# Additional info:
# - confidence_degree: an ordinal variable that was created by Hengl et al.
#   to describe how much one can trust to a particular data sample (expert's
#   estimate). Lower seems to mean better. Probably not good as input variable,
#   but can be probably used as wighting for data samples

# Actual target variables
# - oc: organic carbon
# - n_tot: total nitrogen
# - ph_h2o: pH-Value


clean_target <-
    clean_target[, c("uuid", "olc_id", "site_obsdate", "source_db",
                     "longitude_decimal_degrees",  "latitude_decimal_degrees",
                     "layer_sequence.f", "hzn_top", "hzn_bot", "hzn_depth",
                     "confidence_degree", "oc", "n_tot", "ph_h2o")]

# In some variables there are both empty values and NA values. There seems to be
# an inconsistence in different sources. We replace all empty values with NA
clean_target[clean_target==""]<-NA
# n_tot has string "NaN"
clean_target[ clean_target == "NaN" ] <- NA



# Now we also want to add input variables. Input variables are various satelite
# observations of the earth. These are also called (global) covariate layers or
# just layers (not to be confused with variable layer_sequence.f).

# Each layer provides a number for particular coordinates. So,
# we can either assign each data sample a number from each layer (as authors
# of the paper did) or a reactangular space with the location of the data sample
# in th middle (basically assign a photo, what is our aim). Here, we first
# assign only one number per data sample and layer.

# In the original paper, the authors use a lot of different layers. Partially,
# these layers are available through https://www.openlandmap.org and the
# corresponding API. Openlandmap is an NPO that is driven by the same people
# who wrote the paper we start with.
# Unfortunately, especially high-resolution (30m) layers are not publicaly
# available.
# On the other hand, Openlandmap contains much more layers as were used in the
# paper. In fact, it contains ca. 1.300 layers. Interestingely, even though
# the authors of the paper had direct access to all these layers, they did not
# use all of them in their paper. So, one can assume that the authors did some
# preselection based on their expert knowledge.

# Proposal: In the very beginning, we should use only those layers that were
# used in the paper AND are available on Openlandmap. This intersaction of layers
# is documented in Global_covariate_layers.csv (created by Paul). There are
# 92 layers (input variables). Later we can either use more layers from
# Openlandmap or look how to access high-resolution data from other sources or
# both

# All layers from the paper are documented in
# https://static-content.springer.com/esm/art%3A10.1038%2Fs41598-021-85639-y/MediaObjects/41598_2021_85639_MOESM1_ESM.pdf

# All layers from the Openlandmap are documented in
# https://gitlab.com/openlandmap/global-layers/-/blob/master/tables/openlandmap_wasabi_files.csv

# Description of layers used here:

# - Precipitation monthly in mm. Based on the SM2RAIN-ASCAT 2007-2018, GPM
#   (IMERG) 2014-2018 and WorldClim v2, CHELSA rainfall monthly images.
#   12 Layers:
#       clm_precipitation_sm2rain.apr_m_1km_s0..0cm_2007..2018_v0.2.tif
#       clm_precipitation_sm2rain.aug_m_1km_s0..0cm_2007..2018_v0.2.tif
#       clm_precipitation_sm2rain.dec_m_1km_s0..0cm_2007..2018_v0.2.tif
#       clm_precipitation_sm2rain.feb_m_1km_s0..0cm_2007..2018_v0.2.tif
#       clm_precipitation_sm2rain.jan_m_1km_s0..0cm_2007..2018_v0.2.tif
#       clm_precipitation_sm2rain.jul_m_1km_s0..0cm_2007..2018_v0.2.tif
#       clm_precipitation_sm2rain.jun_m_1km_s0..0cm_2007..2018_v0.2.tif
#       clm_precipitation_sm2rain.mar_m_1km_s0..0cm_2007..2018_v0.2.tif
#       clm_precipitation_sm2rain.may_m_1km_s0..0cm_2007..2018_v0.2.tif
#       clm_precipitation_sm2rain.nov_m_1km_s0..0cm_2007..2018_v0.2.tif
#       clm_precipitation_sm2rain.oct_m_1km_s0..0cm_2007..2018_v0.2.tif
#       clm_precipitation_sm2rain.sep_m_1km_s0..0cm_2007..2018_v0.2.tif

# - USGS Global Ecophysiography landform classification and lithological map.
#   2 Layers:
#       dtm_landform_usgs.ecotapestry_c_250m_s0..0cm_2014_v1.0.tif
#       dtm_lithology_usgs.ecotapestry_c_250m_s0..0cm_2014_v1.0.tif


# - Snow probability monthly. Based on the CCI Land Cover dataset / MOD10A2
#   product at 500 m for the period 2000-2012. 12 Layers:
#       clm_snow.prob_esacci.apr_p_1km_s0..0cm_2000..2016_v1.0.tif
#       clm_snow.prob_esacci.aug_p_1km_s0..0cm_2000..2016_v1.0.tif
#       clm_snow.prob_esacci.dec_p_1km_s0..0cm_2000..2016_v1.0.tif
#       clm_snow.prob_esacci.feb_p_1km_s0..0cm_2000..2016_v1.0.tif
#       clm_snow.prob_esacci.jan_p_1km_s0..0cm_2000..2016_v1.0.tif
#       clm_snow.prob_esacci.jul_p_1km_s0..0cm_2000..2016_v1.0.tif
#       clm_snow.prob_esacci.jun_p_1km_s0..0cm_2000..2016_v1.0.tif
#       clm_snow.prob_esacci.mar_p_1km_s0..0cm_2000..2016_v1.0.tif
#       clm_snow.prob_esacci.may_p_1km_s0..0cm_2000..2016_v1.0.tif
#       clm_snow.prob_esacci.nov_p_1km_s0..0cm_2000..2016_v1.0.tif
#       clm_snow.prob_esacci.oct_p_1km_s0..0cm_2000..2016_v1.0.tif
#       clm_snow.prob_esacci.sep_p_1km_s0..0cm_2000..2016_v1.0.tif

# - Global high-resolution geomorphometry layers - slope, eastness, northness,
#   roughness scale, TWI, MrVBF. 17 layers:
#       dtm_aspect-cosine_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_aspect-sine_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_convergence_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_dev-magnitude_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_dev-scale_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_easthness_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_geom_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_northness_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_pcurv_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_rough-magnitude_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_rough-scale_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_roughness_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_slope_merit.dem_m_250m_s0..0cm_2017_v1.0.tif
#       dtm_tcurv_merit.dem_m_250m_s0..0cm_2018_v1.0.tif
#       dtm_vbf_merit.dem_m_250m_s0..0cm_2017_v1.0.tif
#       dtm_vrm_merit.dem_m_250m_s0..0cm_2018_v1.0.tif

# - MODIS MOD11A2 Land Surface Temperature daytime median values and standard
#   deviation. There are also measurements for day-night difference in
#   temperature. Each value is available on monthly basis. 36 layers:
#       clm_lst_mod11a2.apr.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.apr.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.apr.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.aug.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.aug.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.aug.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.dec.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.dec.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.dec.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.feb.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.feb.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.feb.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.jan.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.jan.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.jan.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.jul.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.jul.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.jul.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.jun.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.jun.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.jun.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.mar.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.mar.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.mar.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.may.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.may.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.may.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.nov.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.nov.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.nov.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.oct.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.oct.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.oct.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.sep.day_m_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.sep.day_sd_1km_s0..0cm_2000..2017_v1.0.tif
#       clm_lst_mod11a2.sep.daynight_m_1km_s0..0cm_2000..2017_v1.0.tif

# - FAPAR (Fraction of Absorbed Photosynthetically Active Radiation) median
#   monthly and annual values 2014-2017 based on the Copernicus PROB-V FAPAR
#   product. 13 layers:
#       veg_fapar_proba.v.annual_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.apr_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.aug_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.dec_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.feb_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.jan_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.jul_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.jun_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.mar_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.may_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.nov_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.oct_d_250m_s0..0cm_2014..2017_v1.0.tif
#       veg_fapar_proba.v.sep_d_250m_s0..0cm_2014..2017_v1.0.tif

# More info on naming convention can be found under:
# https://gitlab.com/openlandmap/global-layers/



# Finally, we use Openlandmap API to get values for layers from
# Global_covariate_layers.csv. Reminder: in the beginning, we assign only
# one number per data sample and layer (NOT a photo)

# First we need relevant coordinates. 
xy.lst = data.frame(lon=clean_target$longitude_decimal_degrees,
                lat=clean_target$latitude_decimal_degrees)

# Load Global_covariate_layers.csv
layers_to_use <- read.csv("Global_covariate_layers.csv")

# How to access the API, can be found on
# https://gitlab.com/openlandmap/global-layers/-/blob/master/tutorial/OpenLandMap_COG_tutorial.md'
# The code below is based on this tutorial

# ATTENTION: the call to API below needs 3-4 hours. The API seems to be
# relatively slow. Note also that in the current code, API call has no
# parallelization (mc.cores = 1). It is because I get error message
# if I use mc.cores > 1. The message says that it is not allowed in windows.
# Under linux/mac parallelization might work. Also note that if parallelization
# is used it, mc.cores should be <= 10, otherwise the service might block you 
in.tif.lst <-
    paste0("/vsicurl/https://s3.eu-central-1.wasabisys.com/openlandmap/",
           layers_to_use$prefix, "/", layers_to_use$global_covariate_layer)

response = parallel::mclapply(in.tif.lst,
                              function(i){terra::extract(rast(i), xy.lst)},
                              mc.cores = 1)
response = as.data.frame(lapply(response, function(i){i[,2]}))
names(response) = basename(in.tif.lst)

clean_target <- cbind(clean_target, response)

# Save the result
write.csv(clean_target,"Target_and_input.csv", row.names = FALSE,
          quote = FALSE)
