##################################################################
# Script developed by Jen Cruz to estimate ranges using AKDE     # 
# For this script we rely on Fleming et al.(2015) Ecology 96(5):1182-1188#
# We use ctmm first, and then use amt                           #
# For instructions on how to use ctmm directly check out:       #
# https://cran.r-project.org/web/packages/ctmm/vignettes/variogram.html #
# https://cran.r-project.org/web/packages/ctmm/vignettes/akde.html #
###################################################################

# THINNED DATA FOR 1-2 INDV

################## prep workspace ###############################

# Clean your workspace to reset your R environment. #
rm( list = ls() )
#install.packages( "ctmm" )

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( amt )
library( sf )
library(ctmm )#for more detailed functionality 
#####################################################################
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
getwd()
#load cleaned data:
#download full data for breeding season monitoring of prairie #
# falcons at the Birds of Prey NCA
trks.breed <- read_rds( "BM_HW3_rangesAKDE/Data/trks.breed" )
#view
trks.breed
#download the thinned (30min) data
trks.thin <- read_rds( "BM_HW3_rangesAKDE/Data/trks.thin" )
#view
trks.thin
#check class
class( trks.thin )
#check that the crs was correctly imported 
get_crs( trks.thin )

###############################################################
##### Estimate ranges using AKDE continuous-time movement model:#
################################################################

# We start by plotting points for each individual:
#
#you can choose which dataset by uncommenting and commenting lines 51 and 52 respectively
#trks.breed %>% 
trks.thin %>% 
  group_by( id ) %>% 
  as_sf_points() %>% 
  summarise( do_union = FALSE ) %>%
  st_cast("LINESTRING" ) %>% 
  #uncommment the line below if you want to view one individual
  #filter( id == 1 ) %>% 
  ggplot(., aes( color = as.factor(id)
                 ) ) +
  theme_bw( base_size = 15 ) + 
  geom_sf() 

####################################################################
############## estimating ranges in CTMM ###########################
###################################################################

######### variograms using ctmm ###############
# We can use ctmm to explore the remaining autocorrelation in our data #
#Compare variograms for thinned and unthinned data for 1 individual 
# 

#choose an individual id:
i <- 7
# filter  tracks to select that individual's data
t <- trks.thin %>% filter( id == i )
#a  <- trks.breed %>% filter( id == i )
#convert to ctmm object
ctmm.t <- as_telemetry( t )
#ctmm.a <- as_telemetry( a )
#estimate empirical variograms
svf.t <- variogram( ctmm.t )
#svf.a <- variogram( ctmm.a )

# DONT RUN FOR NOW

# #now plot them side by side
# par(mfrow = c(2,1) )
# plot(svf.t, fraction = 1, level = 0.95)
# plot(svf.a, fraction = 1, level = 0.95 )
# #now zoom in to starting time lags
# par(mfrow = c(2,1) )
# plot( svf.t, xlim = c(0,2 %#% "day"), 
#      fraction = 1, level = 0.95 )
# plot(svf.a, xlim = c(0,2 %#% "day"), 
#      fraction = 1, level = 0.95 )

#The high resolution plots took a long time on my computer....
# Also note that it includes both sampling rates, the 30min and 
# the 5 sec resolutions. Uneven sampling for the same animal
# makes it hard to determine autocorrelation. We could then 
# subsample to only our fast tracks using the burst_ id column
# Tally how many bursts with > 1 point (which have fast tracks)

#extract only fast tracks (burst) 
#first we calculate number of points per burst
keep <- t %>%  group_by( burst_ ) %>% 
  tally() %>% 
  #filter to keep burst ids when > 10 points in burst
  dplyr:: filter( n > 1 ) 
#use those ids to filter your data
fast <- t %>% filter( burst_ %in% keep$burst_ )
#check
dim(t); dim(fast)
head( fast)
#now we replot our variogram 
ctmm.f <- as_telemetry( fast )
svf.f <- variogram( ctmm.f )
par(mfrow = c(2,1) )
plot(svf.f, fraction = 1, level = 0.95)
plot( svf.f, xlim = c(0,2 %#% "day"), 
      fraction = 1, level = 0.95 )

##### ALL individuals using ctmm     ###############
#Plot variograms for all individuals
# extract names for individuals first into an object
# COMPARING INDIVIDUAL 7 AND 2
ids <- list(7,2)
#create objects to store results
svf.t <- list()
ctmm.t <- list()
xlimz <- c(0,36 %#% "hour" )
#set plot parameters
par( mfrow = c(3,3))
#loop through all individuals 
for( i in 1:length(ids) ){
  #print progress
  print( i )
  # extract data for individual i
  t <- trks.thin %>% filter( id == i )
  #convert to ctmm object and add to list
  ctmm.t[[i]] <- as_telemetry( t )
  #Calculate empirical variograms:
  svf.t[[i]] <- variogram( ctmm.t[[i]] )
  #plot variograms for each individual
  plot( svf.t[[i]], xlim =  xlimz )
}
# Note how they are unique for each individual. This supports 
# the authors suggestions to estimate unique movement models
# for each individual

# ### fast resolution fix rate ####
# the fast resolution tracks have two sampling rates 30min and 5sec
# we can account for variable sampling rates for each individual in 
# our estimates of the variogram as follows: 
#create objects to store results
svf.f <- list()
ctmm.f <- list()
xlimz <- c(0,36 %#% "hour" )
#set plot parameters
par( mfrow = c(3,3))
#loop through all individuals 
for( i in 1:length(ids)){
  #print progress
  print( i )
  # extract data for individual i
  t <- trks.breed %>% filter( id == i )
  #set variable sampling interval
  dt <- c(5, 1800) %#% "second"
  #convert to ctmm object and add to list
  ctmm.f[[i]] <- as_telemetry( t )
  #Calculate empirical variograms:
  svf.f[[i]] <- variogram( ctmm.f[[i]], dt = dt )
  #plot variograms for each individual
  plot( svf.f[[i]], xlim =  xlimz )
}
### how are these varioagrams different with the different 
# resolutions? 
# Answer: THE VARIOGRAMS SHOW THE SAME GENERAL MOVEMENT TRENDS BETWEEN THE COARSE AND
# FINE DATA, HOWEVER, THE FINE DATA CLEARLY ILLUSTRATES THE INDIVIDUAL MOVEMENTS 
# MADE AND IS MORE VARIABLE (AS EXPECTED). 
#

# automate the process of estimating a suitable movement     #
# model for the observed data using the empirical            # 
# variogram as a guide.                                     #
# options are "iid": for uncorrelated independent data,      #
#  "bm": Brownian motion, "ou": Ornstein-Uhlenbeck process,  #
# "ouf": Ornstein-Uhlenbeck forage process,                  #
# "auto": uses model selection with AICc to find bets model  #
# These model choices have real consequences to inference    #

# TELL WHICH ONE REDUCES ERROR 


# for efficiency in the class I'm sticking to the thinned #
# 30min data, moving forward. But you can choose high resolution #
# using the fast variograms we created svf.f #


#create and object to store results 
m.best <- list()
#loop through each individual
#this won't be fast...remember that we are estimating all #
# possible movement models for each individual and then #
# using AIC to pick a best model from the model choices #
# we also plot the empirical variograms vs the model results #
for( i in 1:length(ids)){
  print( i )
  #use empirical variogram estimated in the previous step 
  # as a way of guiding the choice of movement model
  guess <- ctmm.guess(data = ctmm.t[[i]], variogram = svf.t[[i]],
                      interactive = FALSE )
  #here we actually compare among 6 movement model options 
  # and compare fit using AIC to select the top model
  m.best[[i]] <- ctmm.select( ctmm.t[[i]], guess, verbose = TRUE,
                              trace = 2 )
  #view summary output for model comparison for each individual
  print(summary( m.best[[i]] )) 
}
#use individual names to replace those in the list:
names( m.best ) <- ids#[1:2]

#define plotting parameters:
par(mfrow = c(2,2))
#Now compare top model choice against traditional KDE
for( i in 1:length(ids) ){
  #trace progress:
  print(i)
  # add basic IID model to model list
  m.best[[i]]$"IID isotropic" <- ctmm.fit( ctmm.t[[i]],
                                    ctmm(isotropic = TRUE) )
  
  #extract model name for top model
  an <- rownames(summary( m.best[[i]][1]))
  #plot best model
  ctmm::plot( svf.t[[i]], m.best[[i]][[1]], 
        main = paste( ids[i], an ) )#best model
  #plot non-autocorrelated KDE
  ctmm::plot( svf.t[[i]], m.best[[i]]$"IID isotropic", 

        main = paste( ids[i], "IID" ) ) 
}  
# Now that we have estimated top movement models for each #
# individual we are ready to apply those models to our estimates #
# of ranges. We also have an extra option to choose from #
# we can weight points based on high utilisation to correct the range #
# estimate...refer to the manuscript or the vignettes for more details #

# Here we compare ranges from 3 options: (1) top movement model 
# weighted (2) top movement model without weighing (3) traditional kde no weighing

# we create objects to store output from our 3 options:
akde.uw <- list()
akde.w <- list()
kde.iid <- list()
# We loop through each individual to estimate ranges for each option:
for( i in 1:length(ids) ){
  print(i)
  # using the top movement model without weights
  akde.uw[[i]] <- ctmm::akde( ctmm.t[[i]], m.best[[i]][[1]] )
  # using the top movement model with weights
  akde.w[[i]] <- ctmm::akde( ctmm.t[[i]], m.best[[i]][[1]], 
                        weights = TRUE )
  #using the IID movement model without weights
  kde.iid[[i]] <- ctmm::akde( ctmm.t[[i]], m.best[[i]]$"IID isotropic" )
}
#plot estimate ranges comparing output for each option:
par(mfrow = c(3,2))
for( i in 1:length(ids) ){# 2
  print(i)
  plot( ctmm.t[[i]], akde.w[[i]] )
  title( paste("Weighted best model", ids[i]) )
  plot( ctmm.t[[i]], akde.uw[[i]] )
  title("Unweighted best model")
  plot( ctmm.t[[i]], kde.iid [[i]])
  title("Traditional KDE" )
}

akde.uw.t <- akde.uw
akde.w.t <- akde.w
kde.iid.t <- kde.iid

#Plot points for those individuals
#trks.breed %>% 
trks.thin %>%   
  dplyr::filter( id %in% ids ) %>% 
  ggplot(., aes( x = x_, y = y_ ) ) +
  theme_bw( base_size = 15 ) + 
  geom_point() +
  facet_wrap( ~id, scales = "free" )

#extract mean HR estimates for weighted approach as sf polygon
# and combine 
w_list <- list()

for( i in 1:length(ids) ){
  #extract home range for each animal and turn into sf object
  sf.w <- as.sf( akde.w[[i]] )
  # convert crs to study area (otherwise their crs won't match)
  sf.w.t <- st_transform( sf.w, crs = get_crs( trks.thin ) ) 
  #extract only the point estimate (mean range) and add to list
  w_list[[i]] <- sf.w.t[2,]
}

weighted_akdes <-  w_list %>%  dplyr::bind_rows()

weighted_akdes.t <- weighted_akdes
##############################################################
# Estimating AKDE using atm package                          #
##############################################################
################
# We use amt package (talks to ctmm) to estimate AKDE for a #
# single individual to check for computational efficiency #

# define the individual id as an object at the start so you can 
# easily change it and try new ones, without having to alter the #
# rest of the code

################## INDIVIDUAL 7 ##################
i <- 7
#filter data
idv.thin <- trks.thin %>% filter( id == i )
#inspect details for the chosen animal
idv.thin

# start estimating traditional KDE model
f.t.iid <- amt::fit_ctmm( idv.thin, "iid" )
#check
summary( f.t.iid )

#now fit the model chosen as top model by ctmm
f.t.ouf <- amt::fit_ctmm( idv.thin, "ouf" )
#summary values
summary( f.t.ouf )
#What is the run time for that individual?
# Answer: ~ 5 seconds
#
#Estimate ranges for that individual using the KDE 
# approach that assumes no autocorrelation (traditional KDE)
akde_iid <- idv.thin %>% 
  amt::hr_akde(., model = f.t.iid, #fit_ctmm(., "iid" ),
               levels = 0.95 )

#run the ouf model
akde_ouf <- idv.thin %>% 
  amt::hr_akde(., model = f.t.ouf, #fit_ctmm(., "ouf" ),
               weights = TRUE,
               levels = 0.95 )
### the weights option doesn't work eventhough no error is given#####
# How do I know? I rerun without weights and got the exact same answer. #

#Estimate areas for each method
ao <- amt::hr_area( akde_ouf ) 
ai <- amt::hr_area( akde_iid ) 

# comment on the results
# Answer:
# 
# CALCULATING DIFFERENCE BETTWEN UPPER AND LOWER CI AND ESTIMATE
# OUF 
(ao$area[2] - ao$area[1] ) / 1e6 # 145km2
(ao$area[3] - ao$area[2] ) / 1e6 # 160km2
ao$area[2] / 1E6 #811km2

# TRAD KDE
(ai$area[2] - ai$area[1] ) / 1e6 # 90km2
(ai$area[3] - ai$area[2] ) / 1e6 # 96km2
ai$area[2] / 1E6 #754km2

# DIFFERENCE BETWEEN ESTIMATES 
(ao$area[2] - ai$area[2] ) / 1e6 # 57km2

# ID = 7 
# THE BEST MODEL (OUF) HAD A LARGER DIFFERENCE BETWEEN THE ESTIMATE AND 
# THE UPPER AND LOWER CI VALUES THAN THE TRADITIONAL KDE, AND THE DIFFERENCE 
# BETWEEN THE ESTIMATES WAS 57KM2


#Plot comparisons from the different data choices
ggplot() +
  theme_bw( base_size = 15 ) +
  # #extract isopleths for AKDE estimates from
  # #model selection approach:
  # geom_sf( data = hr_isopleths( akde_auto ),
  #          fill = NA, col = "blue", size = 1 ) +
  #extract isopleths for ouf model
  geom_sf( data = hr_isopleths( akde_ouf ),
           fill = NA, col = "orange", size = 3 ) +
  #extract isopleths for traditional kde:
  geom_sf( data = hr_isopleths( akde_iid ), 
           fill = NA, col = "blue", size = 2 ) +
  #add points of autocorrelated data
  # #note that we turn them into sf points for plotting
  geom_sf( data = as_sf_points( subset( trks.thin, id == i ) ) ) 
# +
#   geom_sf( data = as.sf( akde.w[[i]] ), 
#            fill = NA, col = "purple", size = 2 )

# IN ABOVE LINES "Error in akde.w[[i]] : subscript out of bounds
# USING akde.w[[1]] ADDS THE DATA TO THE MAP BUT THE PROJECTION
# IS INCORRECT EVEN THOUGH IT IS DEFINED CORRECTLY WHEN THE OBJECT
# I CREATED 

###################### INDIVIDUAL 2 #######################
i <- 2
#filter data
idv.thin <- trks.thin %>% filter( id == i )
#inspect details for the chosen animal
idv.thin

# start estimating traditional KDE model
f.t.iid <- amt::fit_ctmm( idv.thin, "iid" )
#check
summary( f.t.iid )

#now fit the model chosen as top model by ctmm
f.t.ouf <- amt::fit_ctmm( idv.thin, "ouf" )
#summary values
summary( f.t.ouf )
#What is the run time for that individual?
# Answer: ~ 5 seconds
#
#Estimate ranges for that individual using the KDE 
# approach that assumes no autocorrelation (traditional KDE)
akde_iid <- idv.thin %>% 
  amt::hr_akde(., model = f.t.iid, #fit_ctmm(., "iid" ),
               levels = 0.95 )

#run the ouf model
akde_ouf <- idv.thin %>% 
  amt::hr_akde(., model = f.t.ouf, #fit_ctmm(., "ouf" ),
               weights = TRUE,
               levels = 0.95 )
### the weights option doesn't work eventhough no error is given#####
# How do I know? I rerun without weights and got the exact same answer. #

#Estimate areas for each method
ao <- amt::hr_area( akde_ouf ) 
ai <- amt::hr_area( akde_iid ) 

# comment on the results
# Answer:
# 
# CALCULATING DIFFERENCE BETTWEN UPPER AND LOWER CI AND ESTIMATE
# OUF 
(ao$area[2] - ao$area[1] ) / 1e6 # 44km2
(ao$area[3] - ao$area[2] ) / 1e6 # 48km2
ao$area[2] / 1E6 #294km2

# TRAD KDE
(ai$area[2] - ai$area[1] ) / 1e6 # 36km2
(ai$area[3] - ai$area[2] ) / 1e6 # 39km2
ai$area[2] / 1E6 #293km2

# DIFFERENCE BETWEEN ESTIMATES 
(ao$area[2] - ai$area[2] ) / 1e6 # 0.7km2


# id = 2
# THE DIFFERENCES BETWEEN THE ESTIMATE AND UPPER/LOWER CI VALUES WERE SLIGHTLY 
# LARGER FOR THE OUF MODEL, BUT ONLY MARGINALLY SO. THE ESTIMATES DIFFERED BY 
# ONLY 0.7 KM2



#Plot comparisons from the different data choices
ggplot() +
  theme_bw( base_size = 15 ) +
  # #extract isopleths for AKDE estimates from
  # #model selection approach:
  # geom_sf( data = hr_isopleths( akde_auto ),
  #          fill = NA, col = "blue", size = 1 ) +
  #extract isopleths for ouf model
  geom_sf( data = hr_isopleths( akde_ouf ),
           fill = NA, col = "orange", size = 3 ) +
  #extract isopleths for traditional kde:
  geom_sf( data = hr_isopleths( akde_iid ), 
           fill = NA, col = "blue", size = 2 ) +
  #add points of autocorrelated data
  # #note that we turn them into sf points for plotting
  geom_sf( data = as_sf_points( subset( trks.thin, id == i ) ) ) 
# +
#   geom_sf( data = as.sf( akde.w[[i]] ),
#            fill = NA, col = "purple", size = 2 )

# IN ABOVE LINES "Error in akde.w[[i]] : subscript out of bounds
# USING akde.w[[2]] ADDS THE DATA TO THE MAP BUT THE PROJECTION
# IS INCORRECT EVEN THOUGH IT IS DEFINED CORRECTLY WHEN THE OBJECT
# IS CREATED 


trks.new <- trks.thin %>%
  filter((id %in% c(7,2)))

##### What if you want to use atm for all individuals? #######
##### don't try this in class. It will take a long time #
akde_all <- trks.new %>% nest( data = -"id" ) %>% 
  mutate( hr_akde_all = map( data, ~hr_akde( ., 
              model = fit_ctmm(., "ouf" ),
                levels = 0.95 ) ) )
# 
amt::hr_area( akde_all[[3]][[1]] ) 
amt::hr_area( akde_all[[3]][[2]] ) 


###########################################################
### Save desired results #
# we can save the movement model results
#save range results for 3 home range options
# save( akde.w,file="../ctmm_akde_w.rda")
# save( akde.uw,file="../ctmm_akde_uw.rda")
# save( kde.iid,file="../ctmm_akde_iid.rda")

#save range for all individuals in atm
#write_rds( akde_all, "Data/akde_all" )
#save range for all individuals estimated with ctmm
write_rds( weighted_akdes, "BM_HW3_rangesAKDE/Data/weighted_akdes" )
#save workspace if in progress
save.image( 'AKDEresults.RData' )
############# end of script  ##################################
