#################################################################
# Script developed by Jen Cruz to clean and format location data #
# We also convert cleaned location data to tracks using atm package   # 
# We rely heavily on amt getting started vignette here:       #
# https://cran.r-project.org/web/packages/amt/vignettes/p1_getting_started.html#
##################################################################

################## Prep. workspace ###############################

# Install new packages from "CRAN" repository if you don't have them. # 
install.packages( "tidyverse" ) #actually a collection of packages 
install.packages( "amt" )
#trying to install amt directly from github
# install.packages( "devtools")
# devtools::install_github("jmsigner/amt")
install.packages( "sf" )

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation and plotting
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( amt ) #creating tracks from location data
library( sf ) #handling spatial data
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
getwd()
# if so then:
workdir <- getwd()

# set path to where you can access your data #
datapath <-  paste( getwd(), "/SG_Data/", sep = "" )

# load observed occurrences:
#import GPS data# 

#  MY DATA CONTAINS ALL INDIVIDUALS IN A SINGLE .CSV" 
mydata <- read.csv( file = paste( datapath, "sg_winter.csv", sep = ""),
                    header = TRUE )
head(mydata)

#import polygon of the study area as sf spatial file:
boundarypath <- paste( datapath, "TW_boundary/", sep = "" )
TW_Shape <- sf::st_read( paste0( boundarypath, "TW_Oct2023_Project.shp") )

##############

#######################################################################
######## cleaning data ###############################################
# Data cleaning is crucial for accurate analysis # 
# Trapping records provide info on when individuals were fitted #
# with transmitters.#

colnames( mydata )

#convert date to correct format using lubridate
mydata$newDateTime <- lubridate::mdy_hms( mydata$DATE_TIME_MST, 
                                    tz = "mst")

# Add a day so that we can ignore records from the trapping day #
# and start only with  those from the following day:
# THIS LINE RNS BUT DOES NOTHING? 
mydata$newDateTime <- mydata$newDateTime + lubridate::days(1)

#convert to day of year
mydata$yday <- lubridate::yday( mydata$newDateTime )


#check 
head( mydata); dim( mydata)


###################################################################
# Clean GPS data
# GPS units often provide information on the quality of the fixes they #
# obtained.#
# The units from Cellular track technologies provide HDOP, VDOP and #
# time to fix information # 
# Start by viewing what those look like in the dataset #

# MY DATA HAS NO ACCURACY INFO, WORKING TO SEE IF I CAN GET THIS 
 
# We also need to set a time column containing date and time information #
# in POSIX format (as required by amt)#
# We rely on lubridate for this. If you haven't used lubridate before #
# go here: https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html
# to learn more about how to easily manipulate time and dates in R #
# Data are stored in year, month, day, hour, minute, second format in our data. 
# We define correct format with lubridate 

datadf$date <- lubridate::ymd_hms( datadf$GPS_YYYY.MM.DD_HH.MM.SS,
              tz = "UTC" )
datadf$date <- lubridate::with_tz( datadf$date, tz = "MST" )
# and create new column where we convert it to posixct

mydata$ts <- as.POSIXct( mydata$newDateTime )

#view
head( mydata ); dim( mydata )

# # check if any data are missing
all( complete.cases( mydata ) )
# # none so we can move on

# we also add month and day of year information using lubridate
mydata <- mydata %>% 
  mutate( mth = lubridate::month(newDateTime),
          jday = lubridate::yday(newDateTime) )

#subset to only include winter data points 

winterdata <- subset(mydata, mth %in% c(12, 1, 2))
badbirds <- list(28,37,41,42,47,49,38,43,39) # list of birds with seconds 
# sample rate. See below 
winterdata <- winterdata[!(winterdata$UNIQUE_ID %in% badbirds ),]

# We need to remove records for fixes that were recorded before the #
# units were fitted to the animals so we append relevant information #
# from the records dataframe. We do that by combining datadf to records df#

# MY DATA HAS BEEN PREVIOUSLY CLEANED OF ALL DATA PRIOR TO FITTING UNIT 
# ANIMAL 

##################################################################
### Define coordinate system and projection for the data ######
# location data were recorded using WGS84 in lat long #
# We use the epsg code to define coordinate system for our sites #
# How? Google epsg WGS84 # First result should  take you here: #
# https://spatialreference.org/ref/epsg/wgs-84/ 
# where you can find that epgs = 4326 for this coordinate system #
# If you are not familiar with geographic data, vector, rasters & #
# coordinate systems go here: 
# https://bookdown.org/robinlovelace/geocompr/spatial-class.html #
# to learn more. #

# ALL MY DATA IS ALREADY IN WGS84 UTM 12N

# We can also get an idea of the data collection for each individual
# by plotting histograms
#sampling duration
ggplot( winterdata, aes( x = jday, group = UNIQUE_ID ) ) +
  theme_classic( base_size = 15 ) +
  geom_histogram( ) +
  facet_wrap( ~ UNIQUE_ID )


# What do the histograms tell you about the nature of the data #
# Sample size, intensity for different individuals? #
# Answer: 
# OF THE 26 INDIVIDUALS, MOST HAVE DATA THROUGH THE ENTIRE YEAR
# INDIVIDUALS LIKE 39 AND 198 HAVE SOME LARGE GAPS WHICH WILL LIKELY 
# NEED TO BE REMOVED FROM THE DATASET

#
#######################################################################
###### Creating tracks, calculating step lengths and turning angles ###
####              for all individuals at the same time:           #####
########################################################################
#amt requires us to turn data into tracks for further analyses.
trks <- winterdata %>% 
  #make track. Note you can add additional columns to it
  amt::make_track( .y = Northing, .x = Easting, .t = ts, 
    #define columns that you want to keep, relabel if you need:
    id  = UNIQUE_ID , LOCATION_TYPE= LOCATION_TYPE,
    COMMENTS= COMMENTS, mth = mth,jday = jday, newDateTime = newDateTime, 
    crs = 32612 )
trks <- trks %>%  amt::nest( data = -"id" )
#view
trks

# We plot overall paths for each individual:
for( i in 1:dim(trks)[1]){
  a <- as_sf_points( trks$data[[i]] ) %>% 
    ggplot(.) + theme_bw(base_size = 17) +
    labs( title = paste0('id =', trks$id[i]) ) +
    geom_sf(data = TW_Shape, inherit.aes = FALSE ) +
    geom_sf() 
  print(a)
} 
# removed non-winter

# should first keep winter (based on date), 
# then calculate sample rate
# resample
# step function
# right at end, visualize and calculate metrics 
# then subset with polygon
# this gives points for RSF 

# then match to habitat with polygon for rsf 

# Which ones have migration paths?
# Answer: ALL OF THEM 
#
# Any ideas on how to remove migration data?
# Answer: USE WINTER MONTHS (DEC, JAN, FEB)
# IN FUTURE, THIS WILL HAVE TO BE DONE BY EACH INDIVIDUAL
# EACH INDIVIDUAL MIGRATES AT DIFFERENT TIMES AND THIS VARIES
# BY YEAR 
# 

#still need to look at a couple of individuals in more detail
# 
# # # Plot step lengths
# for( i in 1:dim(trks)[1]){
#   a <-  steps( trks$winter[[i]] ) %>%
#     mutate( jday = lubridate::yday( t1_ ) ) %>%
#     group_by( jday ) %>%
#     summarise( sl_ = sum(sl_) ) %>% # may remove log( sum(sl_) 
#     ggplot(.) + theme_bw(base_size = 17) +
#     labs( title = paste0('individual =', trks$id[i]) ) +
#     geom_line( aes( y = sl_, x = jday) )
#   print(a)
# }

# We focus on winter season data:
# Estimate sampling rate for each individual by looping through 
# data using purr function map

sumtrks <- trks %>%  summarize( 
  map( data, amt::summarize_sampling_rate ) )
#view
sumtrks[[1]]

# look at brds with seconds sampling rate after subsetting to winter 
# FIRST 9 BIRDS HAVE SAMPLING RATE OF SECONDS BECAUSE 
# ORIGINAL DATA ONLY INCLUDES YYYY-MM-DD, TIME IS 00:00
# ANIMAL MOVED IN THESE POINTS EACH DAY, BUT THE SAME 00:00
# TIME SKEWS THE SAMPLE RATE 
# DROPPING THESE BIRDS FOR NOW
# LATER, NEED TO REASSESS DATA TO SEE IF THESE POINTS SHOULD BE KEPT

################HERE###################


# Add tibbles with added step lengths calculated by bursts from #
# winter season data:

trks.all <- trks %>% mutate(
  steps = map( data, function(x) 
    x %>%  track_resample( rate = hours(3), 
                           tolerance = hours(1)) %>% 
      steps_by_burst() ) )
#view
trks.all

trks.all[[3]]

# What would be a reasonable rate to resample at?
# Answer: 3 hours 

trks.steps <- trks.all %>% dplyr::select( id, steps ) %>% 
  unnest( cols = steps ) 
head( trks.steps )
#############################################################
########## step lengths and turning angles  ##################
#########################
# We can plot step lengths by:
trks.steps %>%   
  ggplot(.) +
  #geom_density( aes( x = sl_, fill = as.factor(burst_)), alpha = 0.4 ) +
  geom_histogram( aes( x = sl_ ) ) +
  xlab("Step length" ) + 
  #ylim( 0, 0.01 ) + xlim(0, 2000 ) +
  theme_bw( base_size = 19 )  +
  theme( legend.position = "none" ) +
  facet_wrap( ~id, scales = 'free_y' )


unique_ids <- unique(trks.steps$id)

histogram_plots <- list()

# Loop through each unique 'id'
for (i in unique_ids) {
  # Subset data for the current 'id'
  subset_data <- trks.steps[trks.steps$id == i, ]
  
  # Create a histogram for 'sl_' in the subset
  hist_plot <- ggplot(subset_data, aes(x = sl_)) +
    geom_histogram(binwidth = 100) +  
    labs(title = paste("Histogram for id =", i),
         x = "sl_",
         y = "Frequency")
  
  # Add the plot to the list
  histogram_plots[[i]] <- hist_plot
}

# Print individual histograms
for (i in unique_ids) {
  print(histogram_plots[[i]])
}

#What does the plot tell us about the step lengths traveled by the individual?
# Answer: THE MAJORITY OF STEPS ARE SHORT, LESS THAN 100M ROUGHLY 
# WITH MANY 0M STEPS (GPS ERROR OR LEGITIMATE REPEAT LOCATIONS)
# BUT LARGE GAPS IN TIME MAY BE THE CAUSE FOR LARGE STEPS (24,000M)

#
# Turning angles:
trks.steps %>% #filter( id == 1 ) %>% 
  ggplot( .) +
  geom_histogram( aes( x = ta_ ) ) +
  #geom_histogram( aes( x = direction_p ) ) +
  #coord_polar() +
  ylab("Turning angle") + xlab("") + 
  theme_bw( base_size = 19 ) +
  facet_wrap( ~id, scales = 'free_y' )

histogram_plots2 <- list()

# Loop through each unique 'id'
for (i in unique_ids) {
  # Subset data for the current 'id'
  subset_data <- trks.steps[trks.steps$id == i, ]
  
  # Create a histogram for 'ta_' in the subset
  hist_plot <- ggplot(subset_data, aes(x = ta_)) +
    geom_histogram(binwidth = .05) +  
    labs(title = paste("Histogram for id =", i),
         x = "ta_",
         y = "Frequency")
  
  # Add the plot to the list
  histogram_plots2[[i]] <- hist_plot
}

# Print individual histograms
for (i in unique_ids) {
  print(histogram_plots2[[i]])
}
# Is there any evidence of biased movements for this individual?
# Answer: SURPRISINGLY, THE DISTRIBUTION OF TURNING ANGLE APPEARS NORMAL FOR 
# MOST INDIVIDUALS 
# 

#SUBSETTING BASED ON THE BOUNDARY POLYGON 
# Here we rely on TW_Shape, removing records that exist N, E, S, and W

sf::st_bbox( TW_Shape )

#Then use the Eastern-most coordinate to filter out data 
xmax <- as.numeric(st_bbox(TW_Shape)$xmax)   
xmin <- as.numeric(st_bbox(TW_Shape)$xmin)   
ymax <- as.numeric(st_bbox(TW_Shape)$ymax) 
ymin <- as.numeric(st_bbox(TW_Shape)$ymin) 




#subset those tracks less than as breeding and those > as migrating:

polytrks <- trks %>% mutate(
  winter = map( data, ~ filter(., x_ >= xmin & x_ < xmax & y_ >= ymin & y_ < ymax ) ),
  out = map( data, ~ filter(., x_ < xmin | x_ >= xmax | y_ < ymin | y_ >= ymax ) ) )
# view
polytrks
#we check that it worked for our winter data
for( i in 1:dim(polytrks)[1]){
  a <- as_sf_points( polytrks$winter[[i]] ) %>% 
    ggplot(.) + theme_bw(base_size = 17) +
    labs( title = paste0('individual =', polytrks$id[i]) ) +
    geom_sf(data = TW_Shape, inherit.aes = FALSE ) +
    geom_sf() 
  print(a)
} 

####subset to polygon here
# and save the output with unique names 
# match names below 

# We can now unnest the dataframes of interest
#Starting with all winter season data
trks.winter <- polytrks %>% dplyr::select( id, winter ) %>% 
  unnest( cols = winter ) 
head( trks.winter )

#############################################################################
# Saving relevant objects and data ---------------------------------
#save winter season data (not thinned)
write_rds( trks, "SG_Data/trks")

#save winter season data (turned into steps)
write_rds( trks.steps, "SG_Data/trks.steps" )

#save winter season data (thinned)
write_rds( trks.winter, "SG_Data/trks.winter" )


#save workspace in case we need to make changes
save.image( "TracksWorkspace.RData" )

########## end of save #########################
############### END OF SCRIPT ########################################