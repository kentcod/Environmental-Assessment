#This script extracts ACS data from FIPS county code
#Created for ERG by Kent Codding
#last updated 7/3/23

install.packages("tidycensus") #read ACS package
library(tidycensus)

#read COMIDS with corresponding FIPS of Steam powerplants
setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023/Task 2")
steamp <- read.csv("Steam_EA_Receiving_waters_proposal_FIPS.csv")

#get demographic data functions for each new column of df
FIPS.demo.Indian <- function(FIPS){
  state <- FIPS %/% 1000 #modulus gives first 2 digits
  county <- FIPS %% 1000 #remainder function gives last 3 digits
  American.Indian <- get_acs(geography = 'county', variables = "B02001_004", state = state, county = county)
  return(American.Indian$estimate)
}
indian.vec <- sapply(steamp$FIPS, FIPS.demo.Indian) # get indian demographics for each county associated with steamp

FIPS.demo.Asian <- function(FIPS){
  state <- FIPS %/% 1000
  county <- FIPS %% 1000
  Asian <- get_acs(geography = 'county', variables = "B02001_005", state = state, county = county)
  return(Asian$estimate)
}
asian.vec <- sapply(steamp$FIPS, FIPS.demo.Asian)

FIPS.demo.Black <- function(FIPS){
  state <- FIPS %/% 1000
  county <- FIPS %% 1000
  Black <- get_acs(geography = 'county', variables = "B02001_003", state = state, county = county)
  return(Black$estimate)
}
black.vec <- sapply(steamp$FIPS, FIPS.demo.Black)

FIPS.demo.Latino <- function(FIPS){
  state <- FIPS %/% 1000
  county <- FIPS %% 1000
  Latino <- get_estimates(geography = 'county', product = "characteristics", breakdown = "HISP", state = state, county = county)$value[3] #obtains value for hispanic population 2019
  return(Latino)
}
latino.vec <- sapply(steamp$FIPS, FIPS.demo.Latino)


FIPS.demo.Other <- function(FIPS){
  state <- FIPS %/% 1000
  county <- FIPS %% 1000
  Other <- get_acs(geography = 'county', variables = "B02001_007", state = state, county = county)
  return(Other$estimate)
}
other.vec <- sapply(steamp$FIPS, FIPS.demo.Other)

FIPS.demo.White <- function(FIPS){
  state <- FIPS %/% 1000
  county <- FIPS %% 1000
  White <- get_acs(geography = 'county', variables = "B02001_002", state = state, county = county)
  return(White$estimate)
}
white.vec <- sapply(steamp$FIPS, FIPS.demo.White)

FIPS.demo.Total <- function(FIPS){
  state <- FIPS %/% 1000 #modulus gives first 2 digits
  county <- FIPS %% 1000 #remainder function gives last 3 digits
  Total <- get_acs(geography = 'county', variables = "B02001_001", state = state, county = county)
  return(Total$estimate)
}
total.vec <- sapply(steamp$FIPS, FIPS.demo.Total)


#add census vectors to original steamp dataframe
steamp$American.Indian <- indian.vec
steamp$Asian <- asian.vec
steamp$Black <- black.vec
steamp$Latino <- latino.vec
steamp$Other <- other.vec
steamp$White <- white.vec
steamp$Total <- total.vec
#write new df to csv
setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023/Task 3")
write.csv(steamp, "Steam_EA_Receiving_waters_proposal_Demographics.csv", na = "", row.names = F)


#end of export ACS script
#testing with examples done below this line
#get_acs(variable = B19013, county = FIPS) returns median household income
vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT", year = 2019) #median housing income
#plot median housing income by county in vt
library(ggplot2)
library(magrittr)
library(dplyr)
vt %>%
  mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe), width = 0.3, size = 0.5) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2015-2019 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")
# example with demographic data
test <- FIPS.demo(36099)
tib <- tibble(NAME = names(test)[2:8], estimate = test[2:8])

tib %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_point(color = "red", size = 3) +
  labs(title = paste(test[[1]], "County Demographics"),
       subtitle = "2017-2021 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")
#Race/Demographics
#American Indian
Indian <- get_acs(geography = 'county', variables = "B02001_004", state =  "NY")
#Asian
Asian <- get_acs(geography = 'county', variables = "B02001_005", state =  "NY")
#Black
Black <- get_acs(geography = 'county', variables = "B02001_003", state =  "NY")
#Hispanic/Latino
Latino <- get_acs(geography = 'county', variables = "B02001_009", state =  "NY")
#Other, non-Hispanic
Other <- get_acs(geography = 'county', variables = "B02001_007", state =  "NY")
#White
White <- get_acs(geography = 'county', variables = "B02001_002", state =  "NY")

#Total
total <- get_acs(geography = 'county', variables = "B02001_001", state =  "NY")

#NOTE: geography = 'county' returns df with all FIPS codes in state specified

#for 'binning' populations that are colocated by exceeding benchmark,
# calculate percentage of demographics
percent_white_seneca <- (White$estimate[50]/total$estimate[50] * 100)

# testing for additional NHDPlusERG Package items
COMID.demo <- function(COMID){
  suppressWarnings({
    options(digits = 10)
    flowline <- navigate_nldi(list(featureSource = "comid", featureID = COMID)) #use navigate_nldi function from NHDplus tools to pull spatial data from COMID
    test <- unlist(flowline$origin$geometry) #unlist coordinates into 100 length vector
    df <- data.frame(X = test[1:(length(test)/2)], Y = test[(length(test)/2 + 1):length(test)]) #add coords to X, Y df
    sf_df <- st_as_sf(df, coords = c("X", "Y"), crs = st_crs(FIPS)) #coerce dataframe into spatial df with same geom as polygon layer
    overlay <<- st_intersection(sf_df, polygons) #perform spatial overlay with point coords, FIPS
    if(length(overlay$FIPS) == 0){ #checks to make sure points exist in a county (and not on River or Lake between county boundaries)
      overlay <<- st_join(sf_df, polygons, join = st_nearest_feature) #finds nearest county
    }
    codes <- as.numeric(overlay$FIPS) #create numeric vector of FIPS codes
    counts <- table(codes) #create a table showing instances of each code
    mode <- as.numeric(names(counts))[which.max(counts)] #find which name (FIPS codes) has most counts
    #extract demographics from FIPS using tidycensus package
    demolist <- list(FIPS = mode)
    state <- FIPS %/% 1000 #modulus gives first 2 digits
    county <- FIPS %% 1000 #remainder function gives last 3 digits
    American.Indian <- get_acs(geography = 'county', variables = "B02001_004", state = state, county = county)
    demolist$American.Indian <- American.Indian
  })
}
plot.demo <- function(code){
  suppressWarnings({
    demolist <- list(FIPS = code)
    state <- demolist$FIPS %/% 1000 #integer division first 2 digits
    county <- demolist$FIPS %% 1000 #remainder function gives last 3 digits
    American.Indian <- get_acs(geography = 'county', variables = "B02001_004", state = state, county = county)
    demolist$American.Indian <- American.Indian$estimate
    Asian <- get_acs(geography = 'county', variables = "B02001_005", state = state, county = county)
    demolist$Asian <- Asian$estimate
    Black <- get_acs(geography = 'county', variables = "B02001_003", state = state, county = county)
    demolist$Black <- Black$estimate
    Latino <- get_estimates(geography = 'county', product = "characteristics", breakdown = "HISP", state = state, county = county)$value[3] #obtains value for hispanic population 2019
    demolist$Latino <- Latino
    Other <- get_acs(geography = 'county', variables = "B02001_007", state = state, county = county)
    demolist$Other <- Other$estimate
    White <- get_acs(geography = 'county', variables = "B02001_002", state = state, county = county)
    demolist$White <- White$estimate
    Total <- get_acs(geography = 'county', variables = "B02001_001", state = state, county = county)
    demolist$Total <- Total$estimate
    return(tibble())
  })
}
