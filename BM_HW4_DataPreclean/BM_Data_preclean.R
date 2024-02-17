#################################################################
# Script developed by Jen Cruz to clean and format location data #
# Data are Prairie Falcon locations collected during Spring/Summer #
# of 2021 at Morley Nelson Birds of Prey NCA.   

# Vegetation cover  was downloaded from Rangeland Analysis Platform #
# https://rangelands.app/products/ for 2021 and includes #
# % cover for shrub, perennial herbaceous, annual herbaceous #
# tree, litter and bare ground #
# coordinate system is WGS84 EPSG:4326, spatial resolution is 30m #
#################################################################

################## Prep. workspace ###############################
# we will be using new packages:
install.packages( "terra" )
install.packages( "raster" )
install.packages( "rasterVis" )

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation and plotting
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( amt ) #creating tracks from location data
library( sf ) #handling spatial data
library( terra ) # for raster manipulation
library( raster )
library( rasterVis ) #for raster visualization (of raster:: objects)

## end of package load ###############
###################################################################
#### Load or create data -----------------------------------------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

#import polygon of the NCA as sf spatial file:
NCA <- sf::st_read( "BM_HW4_DataPreclean/Data/BOPNCA_shapefile/BOPNCA_Boundary.shp") 
st_crs(NCA)

#alternatively if you want to use the already cropped version:
cover_NCA <- raster::stack( "BM_HW4_DataPreclean/Data/RAPcover2021_NCA.img" )
crs(cover_NCA)
  
#load 30m steps estimated for all individuals
trks.steps <- read_rds( "BM_HW4_DataPreclean/Data/trks.steps30" )

#load range for all individuals calculated in amt, which also
#contains tracks data used to estimate the range
akde_all <- read_rds( "BM_HW3_rangesAKDE/Data/akde_all" ) #uncorrelated ouf model go back and create akde_all for all individuals - rerun last HW
akde_all

df_inds <- read_rds ("BM_HW4_DataPreclean/Data/df_inds")
##############
#######################################################################
######## preparing raster and polygon data ###############################################
# #get coordinates from shapefile

crstracks <- sf::st_crs( NCA )

# this step is crucial. We don't want to match the raster to the #
# polygon. Why?
# Answer: ALL DATA NEED TO BE IN THE SAME PROJECTED COORDINATE SYSTEM. IF THE 
# CRS SOES NOT MATCH, POINTS WILL NOT ALIGN WITH THE RASTER AND MAY RETURN 
# INCORRECT OR N/A VALUES.

# GEOGRAPHIC COORDINATE SYSTEMS *CAN* BE USED TO MEASURE SHORT DISTANCES, BUT
# A PROJECTED COORDINATE SYSTEM SHOULD BE USED WHEN POSSIBLE. ALSO, MANY PACKAGES
# REQUIRE A PROJECTED COORDINATE SYSTEM. 

# IN THE ORIGINAL CODE, WE HAD THE OPTION OF CLIPPING THE RASTER TO THE POLYGON, 
# THEN USING THE CRS FROM THE RASTER TO DEFINE THE CRS OF THE REST OF THE OBJECTS
# WE CREATE. HOWEVER, THE PRE-CLIPPED DATA WAS IN WGS84 
crs(cover_NCA)
# SO I USED THE CRS FROM THE POLYGON TO DEFINE THE CRS FOR THE RASTER AND THE
# REMAINING OBJECTS 
sf::st_crs( NCA )



# PLOT WITH TERRA
terra::plot( cover_NCA )

#Note the range of values for each vegetation. 
#What are they?
# Answer: 
# LAYER_1: 0-100
# LAYER_2: 0-100
# LAYER_3: 0-50

# IM ASSUMING 
# LAYER_1 = ANNUAL
# LAYER_2 = PERENNIAL
# LAYER_3 = SHRUB
# WILL CONFIRM IN CLASS AND RENAME BEFORE PROCEEDING
##############
###############################################################3
######### preparing tracks and step objects  #########################
#####################################################

###### for RSFs #########
#For RSF we want to draw random points inside the home range of 
# each individual
#check homerange data
akde_all

#define how many random points we want to draw per individual:
# as factor that total points will get multiplied by
rn <- 10
#now get random points
df_inds <- akde_all %>%
  mutate(
    rsf_pnts =  map( hr_akde_all,
     ~ random_points(., n = nrow(.$data)*rn, presence=.$data) ) )
#view
df_inds
write_rds( df_inds, "BM_HW4_DataPreclean/Data/df_inds" )


#now unnest the new dataframes to make sure they worked
rsf_pnts <-  df_inds %>% 
  dplyr::select( id, rsf_pnts ) %>% 
  unnest( cols = rsf_pnts ) 
#check
head( rsf_pnts )

# TRUE = ORIGINAL POINT, FALSE = AVAILABLE RANDOM POINT
#unnest tracks also
trks.thin <- df_inds %>% 
  dplyr::select( id, data ) %>% 
  unnest( cols = data ) 
#view
head( trks.thin)
#check that the correct number of points was extracted
dim(rsf_pnts); dim(trks.thin)

#convert to sf object defining coordinate column
r_all_sf <- sf::st_as_sf( rsf_pnts, coords = c("x_", "y_"), 
                          crs = crstracks )
#now transform to predictor crs:
r_all_trans <- sf::st_transform( r_all_sf, st_crs(cover_NCA) )

#extract predictor at 30m resolution:
cover_tracks_30m <- raster::extract( x = cover_NCA, r_all_trans,
                                 method = "simple" )

#check
cover_tracks_30m
#add resolution to the column labels
colnames(cover_tracks_30m) <- paste( colnames(cover_tracks_30m),
                                     "30m", sep = "_" )

### DO NOT RUN - TAKES A LONG TIME 
#Now at 300m following process for one individual above
# cover_tracks_500m <- raster::extract( x = cover_NCA, r_all_trans,
#                                   method = "simple", buffer = 250,
#                                   fun  = mean, na.rm = TRUE )

# cover_tracks_500m CREATED AND SAVED ON SEPERATE DESKTOP, IMPORTING HERE

cover_tracks_500m <- read.table("BM_HW4_DataPreclean/Data/cover_tracks_500m.txt", header =TRUE, sep = "\t")
cover_tracks_500m <- as.matrix(cover_tracks_500m)

#add resolution to the column labels
colnames(cover_tracks_500m) <- paste( colnames(cover_tracks_500m),
                                     "500m", sep = "_" )

# What proportion of our data are missing values
sum( is.na( cover_tracks_30m ))/ length( cover_tracks_30m )
# NONE 
sum( is.na( cover_tracks_500m ))/ length( cover_tracks_500m )
# NONE

# We append our predictor estimates to the original spatial tibble
df_all <- cbind( rsf_pnts, cover_tracks_30m, cover_tracks_500m )
#check
head(df_all)

#Now we readd individual attributes that we want to keep
head( trks.thin )
#create a new df with extracted attributes
iddf <- trks.thin %>% 
  dplyr::select( id, territory, sex ) %>% 
  group_by( id ) %>% 
  slice(1)
#view
iddf

#join to your dataframe
df_all <- left_join( df_all, iddf, by = "id" )
#check
tail( df_all)

################
##################################################################
###### Now for SSF and iSSF ###
##################################################################
#######
# Step analysis is at a finer resolution so we don't need the ranges. #
# Instead we rely directly on the steps we created in cleaningtracks.R#
# Here we  use the 30min resolution for computational efficiency.

head(trks.steps)

# start by nesting data using purr:
steps_all <- trks.steps %>% nest( data = -"id" )
#view
steps_all
# TURNING ANGLE IN RADIANS, NEEDS 3 POINTS

#Draw random steps for each individual

steps_all <- steps_all %>% 
  dplyr::mutate( rnd = lapply( data, function(x){
    amt::random_steps( x ) } ) )


# The default number of random steps drawn per individual is 10.
# By default the random_steps() function fits a tentative #
# gamma distribution to the observed step lengths and a tentative #
# von Mises distribution to the observed turn angles. #
# It then generates random available points by sampling step-lengths #
# and turn angles from these fitted distributions and combining these #
# random steps with the starting locations associated with each observed #
# movement step. #

#now unnest the new dataframes to make sure they worked
stepsdf <- steps_all %>% dplyr::select( id, rnd ) %>% 
  unnest( cols = rnd ) 
#view
head( stepsdf );dim( stepsdf )

# because we are focused on habitat selection, we choose the end 
# of our steps when extracting habitat. 
# We start by turning it to sf object, assigning the correct projection
#Note that we use the second set of coordinates:
steps_sf <- sf::st_as_sf( stepsdf, coords = c("x2_", "y2_"), 
                          crs = crstracks )
#view
steps_sf

# We then transform the crs:
steps_trans <- sf::st_transform( steps_sf, st_crs(cover_NCA) )

#extracting with raster we can used the sf object directly, you also 
# have the choice to use a buffer around each point if you want to increase 
# your resolution:
cover_steps_30m <- raster::extract( x = cover_NCA, steps_trans,
                             method = "simple" )

#check
head(cover_steps_30m)
# What proportion of our data are missing values
print((sum( is.na( cover_steps_30m ))/ length( cover_steps_30m )) * 100)

# 0.1917437 - 19%
#add resolution to the column labels
colnames(cover_steps_30m) <- paste( colnames(cover_steps_30m),
                                      "30m", sep = "_" )

#extract at 100 m resolution
# cover_steps_100m <- raster::extract( x = cover_NCA, 
#                               steps_trans,
#                               method = "simple", buffer = 50, 
#                               fun  = mean, na.rm = TRUE )

# RAN FOR A WHILE, SAVING AND READING IN 
write.table(cover_steps_100m, file = "BM_HW4_DataPreclean/Data/cover_steps_100m.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
cover_steps_100m <- read.table("BM_HW4_DataPreclean/Data/cover_steps_100m.txt", header =TRUE, sep = "\t")
cover_steps_100m <- as.matrix(cover_steps_100m)

#check
head( cover_steps_100m )
# What proportion of our data are missing values
print((sum( is.na( cover_steps_100m ))/ length( cover_steps_100m ))*100)
# 0.06391458 - 6%

#add resolution to the column labels
colnames(cover_steps_100m) <- paste( colnames(cover_steps_100m),
                                    "100m", sep = "_" )
# We append our predictor estimates to the original spatial tibble
df_steps <- cbind( stepsdf, cover_steps_30m, cover_steps_100m )
#check
head( df_steps )

#Now we read individual attributes that we want to keep
#Note that this assumes that the same individuals are #
#found in both your track and step objects:
#join to your dataframe
df_steps <- left_join( df_steps, iddf, by = "id" )
#check
tail( df_steps)
#########
###############################################################
###########    save relevant objects  ########

# If you created it yourself save cropped raster stack
# terra::writeRaster(  cover_NCA, "Data/RAPcover2021_NCA.img",
# overwrite=TRUE )

# Save the tracks and steps dataframe with extracted raster values 
#appended to them

write_rds( df_all, "BM_HW4_DataPreclean/Data/df_all" )

write_rds( df_steps, "BM_HW4_DataPreclean/Data/df_steps" )

#save workspace if in progress
save.image( 'BM_HabSelDataCleanWorkspace.RData' )

################### end of script ##############################
