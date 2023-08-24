#This script bins populations by whether they are co-located with a receiving water that exceeds a benchmark
#Created for ERG by Kent Codding
#last updated 7/27/23

#read in IRW with COMID, FIPS, and Demographic data all previously calculated in task 3
setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023/Task 3")
steamp <- read.csv("Steam_EA_Receiving_waters_proposal_Demographics.csv")

#read in WQ sheets
setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023/Task 4")
wq.as <- read.csv("WQ_As IRW Model_EA Results by PlantRecWater_proposal.csv")
wq.cd <- read.csv("WQ_Cd IRW Model_EA Results by PlantRecWater_proposal.csv")
wq.cu <- read.csv("WQ_Cu IRW Model_EA Results by PlantRecWater_proposal.csv")
wq.hg <- read.csv("WQ_Hg IRW Model_EA Results by PlantRecWater_proposal.csv")
wq.ni <- read.csv("WQ_Ni IRW Model_EA Results by PlantRecWater_proposal.csv")
wq.pb <- read.csv("WQ_Pb IRW Model_EA Results by PlantRecWater_proposal.csv")
wq.se <- read.csv("WQ_Se IRW Model_EA Results by PlantRecWater_proposal.csv")
wq.ti <- read.csv("WQ_Ti IRW Model_EA Results by PlantRecWater_proposal.csv")
wq.zn <- read.csv("WQ_Zn IRW Model_EA Results by PlantRecWater_proposal.csv")

#Determine which COMIDS have WQ exceedances, "roll up" each IRW by ANY exceedance found for ANY pollutant
#begin with baseline as lowest level of pollutant, then other options that are industry favorable
#baseline
baseline.exc <- c() #empty vector to add COMIDS with exceedances
#As
for(COMID in wq.as$COMID){ #traverses all COMIDS within first pollutant (arsenic) DF COMID variable 
  idx <- which(wq.as$COMID == COMID) #establishes index at which each iteration's COMID exists
  if((wq.as$Baseline.Exceed.FW.acute[idx] == 1) | 
     (wq.as$Baseline.Exceed.FW.chronic[idx] == 1) |
     (wq.as$Baseline.Exceed.HH.O[idx] == 1) |
     (wq.as$Baseline.Exceed.HH_WO[idx] == 1) |
     (wq.as$Baseline.Exceed.MCL[idx] == 1) #checks all 5 WQ benchmarks (some pollutants do not have data for all 5)
  ){
    baseline.exc <- append(baseline.exc, COMID) #append each 
  }
} 
#Cd
for(COMID in wq.as$COMID){ #for loop for   other metals follow same structure as first for loop
  idx <- which(wq.cd$COMID == COMID)
  if(((wq.cd$Baseline.Exceed.FW.acute[idx] == 1)  |
      (wq.cd$Baseline.Exceed.FW.chronic[idx] == 1) |
      (wq.cd$Baseline.Exceed.MCL[idx] == 1)) & 
     (COMID %in% baseline.exc == FALSE) #makes sure that COMID is not double counted if exceeds in two pollutants
  ){
    baseline.exc <- append(baseline.exc, COMID)
  }
}
#Cu
for(COMID in wq.as$COMID){ #for loop for   other metals follow same structure as first for loop
  idx <- which(wq.cu$COMID == COMID)
  if(((wq.cu$Baseline.Exceed.FW.acute[idx] == 1)  |
      (wq.cu$Baseline.Exceed.FW.chronic[idx] == 1) |
      (wq.cu$Baseline.Exceed.HH_WO[idx] == 1)) & 
     (COMID %in% baseline.exc == FALSE) #makes sure that COMID is not double counted if exceeds in two pollutants
  ){
    baseline.exc <- append(baseline.exc, COMID)
  }
}
#Hg
for(COMID in wq.as$COMID){ #for loop for   other metals follow same structure as first for loop
  idx <- which(wq.hg$COMID == COMID)
  if(((wq.hg$Baseline.Exceed.FW.acute[idx] == 1)  |
      (wq.hg$Baseline.Exceed.FW.chronic[idx] == 1) |
      (wq.hg$Baseline.Exceed.MCL[idx] == 1)) & 
     (COMID %in% baseline.exc == FALSE) #makes sure that COMID is not double counted if exceeds in two pollutants
  ){
    baseline.exc <- append(baseline.exc, COMID)
  }
}
#Ni
for(COMID in wq.as$COMID){ #for loop for   other metals follow same structure as first for loop
  idx <- which(wq.ni$COMID == COMID)
  if(((wq.ni$Baseline.Exceed.FW.acute[idx] == 1)  |
      (wq.ni$Baseline.Exceed.FW.chronic[idx] == 1) |
      (wq.ni$Baseline.Exceed.HH_WO[idx] == 1) |
      (wq.ni$Baseline.Exceed.HH_WO[idx] == 1)) & 
     (COMID %in% baseline.exc == FALSE) #makes sure that COMID is not double counted if exceeds in two pollutants
  ){
    baseline.exc <- append(baseline.exc, COMID)
  }
}
#Pb
for(COMID in wq.as$COMID){ #for loop for   other metals follow same structure as first for loop
  idx <- which(wq.pb$COMID == COMID)
  if(((wq.pb$Baseline.Exceed.FW.acute[idx] == 1)  |
      (wq.pb$Baseline.Exceed.FW.chronic[idx] == 1)) & 
     (COMID %in% baseline.exc == FALSE) #makes sure that COMID is not double counted if exceeds in two pollutants
  ){
    baseline.exc <- append(baseline.exc, COMID)
  }
}
#Se
for(COMID in wq.as$COMID){ #for loop for   other metals follow same structure as first for loop
  idx <- which(wq.pb$COMID == COMID)
  if(((wq.pb$Baseline.Exceed.FW.acute[idx] == 1)  |
      (wq.pb$Baseline.Exceed.FW.chronic[idx] == 1)) & 
     (COMID %in% baseline.exc == FALSE) #makes sure that COMID is not double counted if exceeds in two pollutants
  ){
    baseline.exc <- append(baseline.exc, COMID)
  }
}
#Ti
for(COMID in wq.as$COMID){ #forloop for other metals follow same structure as first for loop
  idx <- which(wq.ti$COMID == COMID)
  if(((wq.ti$Baseline.Exceed.HH.O[idx] == 1)  |
      (wq.ti$Baseline.Exceed.HH_WO[idx] == 1) |
      (wq.ti$Baseline.Exceed.MCL[idx] == 1)) & 
     (COMID %in% baseline.exc == FALSE) #makes sure that COMID is not double counted if exceeds in two pollutants
  ){
    baseline.exc <- append(baseline.exc, COMID)
  }
}
#Zn
for(COMID in wq.as$COMID){ #for loop for   other metals follow same structure as first for loop
  idx <- which(wq.zn$COMID == COMID)
  if(((wq.zn$Baseline.Exceed.FW.acute[idx] == 1)  |
      (wq.zn$Baseline.Exceed.FW.chronic[idx] == 1) |
      (wq.zn$Baseline.Exceed.HH_WO[idx] == 1) |
      (wq.zn$Baseline.Exceed.HH_WO[idx] == 1)) & 
     (COMID %in% baseline.exc == FALSE) #makes sure that COMID is not double counted if exceeds in two pollutants
  ){
    baseline.exc <- append(baseline.exc, COMID)
  }
}
#IRW without baseline exceedances
baseline.without.exc <- c()
for(COMID in wq.as$COMID){
  if(!(COMID %in% baseline.exc)) #if COMID not in exceedances vec, add to without.exc vec
    baseline.without.exc <- append(baseline.without.exc, COMID)
}
#Divide COMIDS by WQ exceed/ not exceed for all Options
Option.1.exc <- c() #create empty vector for each Option to append to later
Option.2.exc <- c()
Option.3.exc <- c()
Option.4.exc <- c()

#Note: when "rolling up" COMIDS that have exceedances for 1+ pollutants, the other 8 pollutants do not have exceedances in any COMIDS without Arsenic exceedances
#As
## Option 1
for(COMID in wq.as$COMID){
  idx <- which(wq.as$COMID == COMID) #matches correct index to each COMID in case COMIDs are not ordered the same for each sheet
  if((wq.as$Option.1.Exceed.FW.Acute[idx] == 1)|
     (wq.as$Option.1.Exceed.FW.Chronic[idx] == 1) |
     (wq.as$Option.1.Exceed.HH.O[idx] == 1) |
     (wq.as$Option.1.Exceed.HH.WO[idx] == 1) |
     (wq.as$Option.1.Exceed.MCL[idx] == 1)#checks all 5 WQ benchmarks (some pollutants do not have data for all 5)
  ){
    Option.1.exc <- append(Option.1.exc, COMID) #append each 
  }
}
Option.1.without.exc <- c()  # Initialize an empty vector
for (COMID in wq.as$COMID) {
  if (!(COMID %in% Option.1.exc)) { #if COMID not in exceedances vec, add to without.exc vec
    Option.1.without.exc <- append(Option.1.without.exc, COMID)
  }
}

## Option 2
for(COMID in wq.as$COMID){
  idx <- which(wq.as$COMID == COMID) #matches correct index to each COMID in case COMIDs are not ordered the same for each sheet
  if((wq.as$Option.2.Exceed.FW.Acute[idx] == 1)|
     (wq.as$Option.2.Exceed.FW.Chronic[idx] == 1) |
     (wq.as$Option.2.Exceed.HH.O[idx] == 1) |
     (wq.as$Option.2.Exceed.HH.WO[idx] == 1) |
     (wq.as$Option.2.Exceed.MCL[idx] == 1)#checks all 5 WQ benchmarks (some pollutants do not have data for all 5)
  ){
    Option.2.exc <- append(Option.2.exc, COMID) #append each 
  }
}
Option.2.without.exc <- c()  # Initialize an empty vector
for (COMID in wq.as$COMID) {
  if (!(COMID %in% Option.2.exc)) { #if COMID not in exceedances vec, add to without.exc vec
    Option.2.without.exc <- append(Option.2.without.exc, COMID)
  }
}
## Option 3
for(COMID in wq.as$COMID){
  idx <- which(wq.as$COMID == COMID) #matches correct index to each COMID in case COMIDs are not ordered the same for each sheet
  if((wq.as$Option.3.Exceed.FW.Acute[idx] == 1)|
     (wq.as$Option.3.Exceed.FW.Chronic[idx] == 1) |
     (wq.as$Option.3.Exceed.HH.O[idx] == 1) |
     (wq.as$Option.3.Exceed.HH.WO[idx] == 1) |
     (wq.as$Option.3.Exceed.MCL[idx] == 1)#checks all 5 WQ benchmarks (some pollutants do not have data for all 5)
  ){
    Option.3.exc <- append(Option.3.exc, COMID) #append each 
  }
}
Options.3and4.without.exc <- c()  # Initialize an empty vector
for (COMID in wq.as$COMID) {
  if (!(COMID %in% Option.3.exc)) { #if COMID not in exceedances vec, add to without.exc vec
    Options.3and4.without.exc <- append(Options.3and4.without.exc, COMID)
  }
}
## Option 4
for(COMID in wq.as$COMID){
  idx <- which(wq.as$COMID == COMID) #matches correct index to each COMID in case COMIDs are not ordered the same for each sheet
  if((wq.as$Option.4.Exceed.FW.Acute[idx] == 1)|
     (wq.as$Option.4.Exceed.FW.Chronic[idx] == 1) |
     (wq.as$Option.4.Exceed.HH.O[idx] == 1) |
     (wq.as$Option.4.Exceed.HH.WO[idx] == 1) |
     (wq.as$Option.4.Exceed.MCL[idx] == 1)#checks all 5 WQ benchmarks (some pollutants do not have data for all 5)
  ){
    Option.4.exc <- append(Option.4.exc, COMID) #append each 
  }
}

#Demographics of IRW with Exceedances vs. IRW without Exceedances 
library(magrittr)#read in commonly used %>% pipe operator

##create empty values of demographics of IRW with Exceedances to sum up later
  reset.demo <- function(){
    American.Indian <<- 0
    Asian <<- 0
    Black <<- 0
    Latino <<- 0
    Other <<- 0
    White <<- 0
    Total <<- 0
  }
  reset.demo()
## function call below after calling above function handily calculates percentages for each demographic
  percent.demo <- function(){ #calculates the fraction of the total population for each demographic
    total <- American.Indian + Asian + Black + Latino + Other + White # calculates total by summing all demographics to ensure accurate percentages
    American.Indian.per <<- American.Indian/total
    Asian.per <<- Asian/total
    Black.per <<- Black/total
    Latino.per <<- Latino/total
    Other.per <<- Other/total
    White.per <<- White/total
  }
## function below extracts FIPS from a COMID from previously calculated sheet Steam_EA_Receiving_waters_proposal_Demographics.csv
  steamp.FIPS <- function(COMID){
    idx <- steamp$COMID == COMID
    code <- steamp$FIPS[idx]
    return(code)
  }
## function below sums demographics for a FIPS code
  FIPS.demo.sum <- function(FIPS) {
    idx <- steamp$FIPS == FIPS
    American.Indian <<- American.Indian + sum(steamp$American.Indian[idx])
    Asian <<- Asian + sum(steamp$Asian[idx])
    Black <<- Black + sum(steamp$Black[idx])
    Latino <<- Latino + sum(steamp$Latino[idx])
    Other <<- Other + sum(steamp$Other[idx])
    White <<- White + sum(steamp$White[idx])
    Total <<- Total + sum(steamp$Total[idx])
    return(Total)
  }
##demographics for Baseline, Option 1-4 for all 9 rolled up WQ pollutants 
  baseline.exceed <- sapply(baseline.exc, steamp.FIPS) %>% unique() #iterates COMID to FIPS function over all COMID that exceed baseline benchmark then extracts unique FIPS values
  # unique() avoids double counting populations in this next step
  baseline.exceed.demo <- sapply(baseline.exceed, FIPS.demo.sum) #iterates sum demographics function for every unique county code
  percent.demo()
  Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(baseline.exc))
  IRW.WQ.Demographics <- data.frame(Percent.Baseline.With.Exceedance)
  reset.demo() #reset demographics for next column in dataframe (baseline without exceed)
  ###Baseline w/o exceedance
  baseline.wo.exceed.FIPS <- sapply(baseline.without.exc, steamp.FIPS) %>% unique() #same procedure to obtain without exceed + Option benchmarks
  baseline.wo.exceed.demo <- sapply(baseline.wo.exceed.FIPS, FIPS.demo.sum)
  percent.demo()
  Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(baseline.without.exc))
  IRW.WQ.Demographics <- data.frame(IRW.WQ.Demographics, Percent.Baseline.Without.Exceedance)
  reset.demo()
  ###Option 1 w/ exceedance
  Option.1.exceed.FIPS <- sapply(Option.1.exc, steamp.FIPS) %>% unique()
  Option.1.exceed.demo <- sapply(Option.1.exceed.FIPS, FIPS.demo.sum)
  percent.demo()
  Percent.Option.1.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(Option.1.exc))
  IRW.WQ.Demographics <- data.frame(IRW.WQ.Demographics, Percent.Option.1.With.Exceedance)
  reset.demo()
  ### Option 1 w/o exceedance
  Option.1.wo.exceed.FIPS <- sapply(Option.1.without.exc, steamp.FIPS) %>% unique()
  Option.1.wo.exceed.demo <- sapply(Option.1.wo.exceed.FIPS, FIPS.demo.sum)
  percent.demo()
  Percent.Option.1.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(Option.1.without.exc))
  IRW.WQ.Demographics <- data.frame(IRW.WQ.Demographics, Percent.Option.1.Without.Exceedance)
  reset.demo()
  ### Option 2 w/ exceedance
  Option.2.exceed.FIPS <- sapply(Option.2.exc, steamp.FIPS) %>% unique()
  Option.2.exceed.demo <- sapply(Option.2.exceed.FIPS, FIPS.demo.sum) 
  percent.demo()
  Percent.Option.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(Option.2.exc))
  IRW.WQ.Demographics$Percent.Option.2.With.Exceedance <- Percent.Option.2.With.Exceedance
  reset.demo()
  ### Option 2 w/o exceedance
  Option.2.wo.exceed.FIPS <- sapply(Option.2.without.exc, steamp.FIPS) %>% unique()
  Option.2.wo.exceed.demo <- sapply(Option.2.wo.exceed.FIPS, FIPS.demo.sum)
  percent.demo()
  Percent.Option.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(Option.2.without.exc))
  IRW.WQ.Demographics$Percent.Option.2.Without.Exceedance <- Percent.Option.2.Without.Exceedance
  reset.demo()
  ### Option 3 and 4 w/ exceedance
  Option.3.exceed.FIPS <- sapply(Option.3.exc, steamp.FIPS) %>% unique()
  Option.3.exceed.demo <- sapply(Option.3.exceed.FIPS, FIPS.demo.sum) 
  percent.demo()
  Percent.Option.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(Option.3.exc))
  IRW.WQ.Demographics$Percent.Option.3.and.4.With.Exceedance <- Percent.Option.3.and.4.With.Exceedance
  reset.demo()
  ### Option 3 and 4 w/o exceedance
  Options.3and4.without.exceed.FIPS <- sapply(Options.3and4.without.exc, steamp.FIPS) %>% unique()
  Option.3.exceed.demo <- sapply(Options.3and4.without.exceed.FIPS, FIPS.demo.sum)
  percent.demo()
  Percent.Option.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(Options.3and4.without.exc))
  IRW.WQ.Demographics$Percent.Option.3.and.4.Without.Exceedance <- Percent.Option.3.and.4.Without.Exceedance
  reset.demo()
  ### rbind unique counts of IRW for all 8 categories
  count.unique.IRW <- c("Percent.Baseline.With.Exceedance" = length(baseline.exceed), "Percent.Baseline.Without.Exceedance" = length(baseline.wo.exceed.FIPS), "Percent.Option.1.With.Exceedance" = length(Option.1.exceed.FIPS), "Percent.Option.1.Without.Exceedance" = length(Option.1.wo.exceed.FIPS), "Percent.Option.2.With.Exceedance" = length(Option.2.exceed.FIPS), "Percent.Option.2.Without.Exceedance" = length(Option.2.wo.exceed.FIPS), "Percent.Option.3.and.4.With.Exceedance" = length(Option.3.exceed.FIPS), "Percent.Option.3.and.4.Without.Exceedance" = length(Options.3and4.without.exceed.FIPS)) 
  rbind(IRW.WQ.Demographics, "Count of Unique FIPS" = count.unique.IRW) -> IRW.WQ.Demographics #add lengths of unique FIPS vectors to bottom of each column through row bind
  ### write df to csv
  write.csv(IRW.WQ.Demographics, "IRW.WQ.Demographics.csv")

#read in sheet for wildlife TEC, NEHC T4, NEHC T3 previously edited to include benchmark exceedance columns in Task 1
  setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023/Task 1")
  wildlife <- read.csv("Updated CSV - wildlife - IRW Model_EA Results.csv")
  ##baseline exceedances
  TEC.exc <- c()
  TEC.without.exc <- c() #TEC exceed and without exceedance vectors to append to 
  NEHCT3.exc <- c()
  NEHCT3.without.exc <- c()
  NEHCT4.exc <- c()
  NEHCT4.without.exc <- c()
  for(COMID in wildlife$COMID){
    idx <- which(wildlife$COMID == COMID) #which finds index where COMID is in wildlife spreadsheet
    if(!(COMID %in% TEC.exc) & (any(wildlife$Exceed.Baseline.HQsed[idx] == 1))) #determine if COMID is already in exceed vec and if COMID HQsed exceeds
      TEC.exc <- c(TEC.exc, COMID)
    if(!(COMID %in% TEC.without.exc) & (!(COMID %in% TEC.exc)))
      TEC.without.exc <- c(TEC.without.exc, COMID)
    if((COMID %in% NEHCT3.exc == FALSE) & any(wildlife$Exceed.Baseline.HQ_T3[idx] == 1))
      NEHCT3.exc <- c(NEHCT3.exc, COMID)
    if((COMID %in% NEHCT3.without.exc == FALSE) & (COMID %in% NEHCT3.exc == FALSE))
      NEHCT3.without.exc <- c(NEHCT3.without.exc, COMID)
    if((COMID %in% NEHCT4.exc == FALSE) & any(wildlife$Exceed.Baseline.HQ_T4[idx] == 1))
      NEHCT4.exc <- c(NEHCT4.exc, COMID)
    if((COMID %in% NEHCT4.without.exc == FALSE) & (COMID %in% NEHCT4.exc == FALSE))
      NEHCT4.without.exc <- c(NEHCT4.without.exc, COMID)
  }
  ##option 1 and 2 exceedances
  TEC.Opt1and2.exc <- c()
  TEC.Opt1and2.without.exc <- c() #TEC exceed and without exceedance vectors to append to 
  NEHCT3.Opt1and2.exc <- c()
  NEHCT3.Opt1and2.without.exc <- c()
  NEHCT4.Opt1and2.exc <- c()
  NEHCT4.Opt1and2.without.exc <- c()
  for(COMID in wildlife$COMID){
    idx <- which(wildlife$COMID == COMID) #which finds index (indeces in this case) where COMID is in spreadsheet
    if(!(COMID %in% TEC.Opt1and2.exc) & (any(wildlife$Exceed.Option.1.HQsed[idx] == 1) | any(wildlife$Exceed.Option.2.HQsed[idx]) == 1)) #determine if COMID is already in exceed vec and if COMID HQsed exceeds
      TEC.Opt1and2.exc <- c(TEC.Opt1and2.exc, COMID)
    if(!(COMID %in% TEC.Opt1and2.without.exc) & (!(COMID %in% TEC.Opt1and2.exc)))
      TEC.Opt1and2.without.exc <- c(TEC.Opt1and2.without.exc, COMID)
    if((COMID %in% NEHCT3.Opt1and2.exc == FALSE) & (any(wildlife$Exceed.Option.1.HQ_T3[idx] == 1) | any(wildlife$Exceed.Option.2.HQ_T3[idx]) == 1))
      NEHCT3.Opt1and2.exc <- c(NEHCT3.Opt1and2.exc, COMID)
    if((COMID %in% NEHCT3.Opt1and2.without.exc == FALSE) & (COMID %in% NEHCT3.Opt1and2.exc == FALSE))
      NEHCT3.Opt1and2.without.exc <- c(NEHCT3.Opt1and2.without.exc, COMID)
    if((COMID %in% NEHCT4.Opt1and2.exc == FALSE) & (any(wildlife$Exceed.Option.1.HQ_T4[idx] == 1) | any(wildlife$Exceed.Option.2.HQ_T4[idx]) == 1))
      NEHCT4.Opt1and2.exc <- c(NEHCT4.Opt1and2.exc, COMID)
    if((COMID %in% NEHCT4.Opt1and2.without.exc == FALSE) & (COMID %in% NEHCT4.Opt1and2.exc == FALSE))
      NEHCT4.Opt1and2.without.exc <- c(NEHCT4.Opt1and2.without.exc, COMID)
  }
  ##Option 3 and 4 Exceedances
  TEC.Opt3and4.exc <- c()
  TEC.Opt3and4.without.exc <- c() #TEC exceed and without exceedance vectors to append to 
  NEHCT3.Opt3and4.exc <- c()
  NEHCT3.Opt3and4.without.exc <- c()
  NEHCT4.Opt3and4.exc <- c()
  NEHCT4.Opt3and4.without.exc <- c()
  for(COMID in wildlife$COMID){
    idx <- which(wildlife$COMID == COMID) #which finds index (indices in this case) where COMID is in spreadsheet
    if(!(COMID %in% TEC.Opt3and4.exc) & (any(wildlife$Exceed.Option.3.HQsed[idx] == 1) | any(wildlife$Exceed.Option.4.HQsed[idx]) == 1)) #determine if COMID is already in exceed vec and if COMID HQsed exceeds
      TEC.Opt3and4.exc <- c(TEC.Opt3and4.exc, COMID)
    if(!(COMID %in% TEC.Opt3and4.without.exc) & (!(COMID %in% TEC.Opt3and4.exc)))
      TEC.Opt3and4.without.exc <- c(TEC.Opt3and4.without.exc, COMID)
    if((COMID %in% NEHCT3.Opt3and4.exc == FALSE) & (any(wildlife$Exceed.Option.3.HQ_T3[idx] == 1) | any(wildlife$Exceed.Option.4.HQ_T3[idx]) == 1))
      NEHCT3.Opt3and4.exc <- c(NEHCT3.Opt3and4.exc, COMID)
    if((COMID %in% NEHCT3.Opt3and4.without.exc == FALSE) & (COMID %in% NEHCT3.Opt3and4.exc == FALSE))
      NEHCT3.Opt3and4.without.exc <- c(NEHCT3.Opt3and4.without.exc, COMID)
    if((COMID %in% NEHCT4.Opt3and4.exc == FALSE) & (any(wildlife$Exceed.Option.3.HQ_T4[idx] == 1) | any(wildlife$Exceed.Option.4.HQ_T4[idx]) == 1))
      NEHCT4.Opt3and4.exc <- c(NEHCT4.Opt3and4.exc, COMID)
    if((COMID %in% NEHCT4.Opt3and4.without.exc == FALSE) & (COMID %in% NEHCT4.Opt3and4.exc == FALSE))
      NEHCT4.Opt3and4.without.exc <- c(NEHCT4.Opt3and4.without.exc, COMID)
  }
#Bin populations by exceedance for all 3 wildlife benchmarks
  ## TEC
    ###baseline
    TEC.exceed.b.FIPS <- sapply(TEC.exc, steamp.FIPS) %>% unique() #iterates COMID to FIPS function over all COMID that exceed baseline benchmark then extracts unique FIPS values
    # unique() avoids double counting populations in this next step
    TEC.exceed.b.demo <- sapply(TEC.exceed.b.FIPS, FIPS.demo.sum) #iterates sum demographics function for every unique county code
    percent.demo() #calculates percentage of total for each demographic
    Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(TEC.exc))
    IRW.wildlife.TEC.Demographics <- data.frame(Percent.Baseline.With.Exceedance) #create df with column 1 as baseline exceedance to append to and eventually export
    reset.demo() #reset demographics for next column in dataframe (baseline without exceed)
    ###Baseline w/o exceedance
    TEC.exceed.b.wo.FIPS <- sapply(TEC.without.exc, steamp.FIPS) %>% unique()
    TEC.exceed.b.wo.demo <- sapply(TEC.exceed.b.wo.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(TEC.without.exc))
    IRW.wildlife.TEC.Demographics$Percent.Baseline.Without.Exceedance <- Percent.Baseline.Without.Exceedance
    reset.demo()
    ###Option 1 and 2 with exceedance
    TEC.Opt1and2.exceed.FIPS <- sapply(TEC.Opt1and2.exc, steamp.FIPS) %>% unique()
    TEC.Opt1and2.exceed.demo <- sapply(TEC.Opt1and2.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.1.and.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(TEC.Opt1and2.exc))
    IRW.wildlife.TEC.Demographics$Percent.Option.1.and.2.With.Exceedance <- Percent.Option.1.and.2.With.Exceedance
    reset.demo()
    ###Option 1 and 2 without exceedance
    TEC.Opt1and2.wo.exceed.FIPS <- sapply(TEC.Opt1and2.without.exc, steamp.FIPS) %>% unique()
    TEC.Opt1and2.wo.exceed.demo <- sapply(TEC.Opt1and2.wo.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.1.and.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(TEC.Opt1and2.without.exc))
    IRW.wildlife.TEC.Demographics$Percent.Option.1.and.2.Without.Exceedance <- Percent.Option.1.and.2.Without.Exceedance
    reset.demo()
    ###Option 3 and 4 with exceedance
    TEC.Opt3and4.exceed.FIPS <- sapply(TEC.Opt3and4.exc, steamp.FIPS) %>% unique()
    TEC.Opt3and4.exceed.demo <- sapply(TEC.Opt3and4.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(TEC.Opt3and4.exc))
    IRW.wildlife.TEC.Demographics$Percent.Option.3.and.4.With.Exceedance <- Percent.Option.3.and.4.With.Exceedance
    reset.demo()
    ###Option 3 and 4 without exceedance
    TEC.Opt3and4.wo.exceed.FIPS <- sapply(TEC.Opt3and4.without.exc, steamp.FIPS) %>% unique()
    TEC.Opt3and4.wo.exceed.demo <- sapply(TEC.Opt3and4.wo.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(TEC.Opt3and4.without.exc))
    IRW.wildlife.TEC.Demographics$Percent.Option.3.and.4.Without.Exceedance <- Percent.Option.3.and.4.Without.Exceedance
    reset.demo()
    ###rbind unique IRWs and write to csv
    count.unique.IRW <- c(length(TEC.exceed.b.FIPS), length(TEC.exceed.b.wo.FIPS), length(TEC.Opt1and2.exceed.FIPS), length(TEC.Opt1and2.wo.exceed.FIPS), length(TEC.Opt3and4.exceed.FIPS), length(TEC.Opt3and4.wo.exceed.FIPS))
    IRW.wildlife.TEC.Demographics <-  rbind(IRW.wildlife.TEC.Demographics, "Count of Unique FIPS" = count.unique.IRW)
    write.csv(IRW.wildlife.TEC.Demographics, "IRW.wildlife.TEC.Demographics.csv")
  ##NEHCT4
    ###Baseline exceedance
    NEHCT4.b.exceed.FIPS <- sapply(NEHCT4.exc, steamp.FIPS) %>% unique()
    NEHCT4.b.exceed.demo <- sapply(NEHCT4.b.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT4.exc))
    IRW.wildlife.NEHCT4.Demographics <- data.frame(Percent.Baseline.With.Exceedance)
    reset.demo()
    ###Baseline without exceedance
    NEHCT4.without.exceed.FIPS <- sapply(NEHCT4.without.exc, steamp.FIPS) %>% unique()
    NEHCT4.without.exceed.demo <- sapply(NEHCT4.without.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT4.without.exc))
    IRW.wildlife.NEHCT4.Demographics$Percent.Baseline.Without.Exceedance <- Percent.Baseline.Without.Exceedance
    reset.demo()
    ### Options 1 and 2 with exceedance
    NEHCT4.Opt1and2.exceed.FIPS <- sapply(NEHCT4.Opt1and2.exc, steamp.FIPS) %>% unique()
    NEHCT4.Opt1and2.exceed.demo <- sapply(NEHCT4.Opt1and2.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT4.Opt1and2.exc))
    IRW.wildlife.NEHCT4.Demographics$Percent.Options.1.and.2.With.Exceedance <- Percent.Options.1.and.2.With.Exceedance
    reset.demo()
    ### Option 1 and 2 without exceedance 
    NEHCT4.Opt1and2.wo.exceed.FIPS <- sapply(NEHCT4.Opt1and2.without.exc, steamp.FIPS) %>% unique() 
    NEHCT4.Opt1and2.wo.exceed.demo <- sapply(NEHCT4.Opt1and2.wo.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT4.Opt1and2.without.exc))
    IRW.wildlife.NEHCT4.Demographics$Percent.Options.1.and.2.Without.Exceedance <- Percent.Options.1.and.2.Without.Exceedance
    reset.demo()
    ### Option 3 and 4 with exceedance
    NEHCT4.Opt3and4.exceed.FIPS <- sapply(NEHCT4.Opt3and4.exc, steamp.FIPS) %>% unique()
    NEHCT4.Opt3and4.exceed.demo <- sapply(NEHCT4.Opt3and4.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT4.Opt3and4.exc))
    IRW.wildlife.NEHCT4.Demographics$Percent.Options.3.and.4.With.Exceedance <- Percent.Options.3.and.4.With.Exceedance
    reset.demo()
    ### Option 3 and 4 without exceedance
    NEHCT4.Opt3and4.wo.exceed.FIPS <- sapply(NEHCT4.Opt3and4.without.exc, steamp.FIPS) %>% unique()
    NEHCT4.Opt3and4.wo.exceed.demo <- sapply(NEHCT4.Opt3and4.wo.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT4.Opt3and4.without.exc))
    IRW.wildlife.NEHCT4.Demographics$Percent.Options.3.and.4.Without.Exceedance <- Percent.Options.3.and.4.Without.Exceedance
    reset.demo()
    ### rbind unique IRW counts then export
    count.unique.IRW <- c(length(NEHCT4.b.exceed.FIPS), length(NEHCT4.without.exceed.FIPS), length(NEHCT4.Opt1and2.exceed.FIPS), length(NEHCT4.Opt1and2.wo.exceed.FIPS), length(NEHCT4.Opt3and4.exceed.FIPS), length(NEHCT4.Opt3and4.wo.exceed.FIPS))
    IRW.wildlife.NEHCT4.Demographics <- rbind(IRW.wildlife.NEHCT4.Demographics, "Count of Unique FIPS" = count.unique.IRW)
    write.csv(IRW.wildlife.NEHCT4.Demographics, "IRW.wildlife.NEHCT4.Demographics.csv")
  ## NEHCT3
    ###Baseline exceedance
    NEHCT3.b.exceed.FIPS <- sapply(NEHCT3.exc, steamp.FIPS) %>% unique()
    NEHCT3.b.exceed.demo <- sapply(NEHCT3.b.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT3.exc))
    IRW.wildlife.NEHCT3.Demographics <- data.frame(Percent.Baseline.With.Exceedance)
    reset.demo()
    ###Baseline without exceedance
    NEHCT3.without.exceed.FIPS <- sapply(NEHCT3.without.exc, steamp.FIPS) %>% unique()
    NEHCT3.without.exceed.demo <- sapply(NEHCT3.without.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT3.without.exc))
    IRW.wildlife.NEHCT3.Demographics$Percent.Baseline.Without.Exceedance <- Percent.Baseline.Without.Exceedance
    reset.demo()
    ### Options 1 and 2 with exceedance
    NEHCT3.Opt1and2.exceed.FIPS <- sapply(NEHCT3.Opt1and2.exc, steamp.FIPS) %>% unique()
    NEHCT3.Opt1and2.exceed.demo <- sapply(NEHCT3.Opt1and2.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT3.Opt1and2.exc))
    IRW.wildlife.NEHCT3.Demographics$Percent.Options.1.and.2.With.Exceedance <- Percent.Options.1.and.2.With.Exceedance
    reset.demo()
    ### Option 1 and 2 without exceedance 
    NEHCT3.Opt1and2.wo.exceed.FIPS <- sapply(NEHCT3.Opt1and2.without.exc, steamp.FIPS) %>% unique() 
    NEHCT3.Opt1and2.wo.exceed.demo <- sapply(NEHCT3.Opt1and2.wo.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT3.Opt1and2.without.exc))
    IRW.wildlife.NEHCT3.Demographics$Percent.Options.1.and.2.Without.Exceedance <- Percent.Options.1.and.2.Without.Exceedance
    reset.demo()
    ### Option 3 and 4 with exceedance
    NEHCT3.Opt3and4.exceed.FIPS <- sapply(NEHCT3.Opt3and4.exc, steamp.FIPS) %>% unique()
    NEHCT3.Opt3and4.exceed.demo <- sapply(NEHCT3.Opt3and4.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT3.Opt3and4.exc))
    IRW.wildlife.NEHCT3.Demographics$Percent.Options.3.and.4.With.Exceedance <- Percent.Options.3.and.4.With.Exceedance
    reset.demo()
    ### Option 3 and 4 without exceedance
    NEHCT3.Opt3and4.wo.exceed.FIPS <- sapply(NEHCT3.Opt3and4.without.exc, steamp.FIPS) %>% unique()
    NEHCT3.Opt3and4.wo.exceed.demo <- sapply(NEHCT3.Opt3and4.wo.exceed.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(NEHCT3.Opt3and4.without.exc))
    IRW.wildlife.NEHCT3.Demographics$Percent.Options.3.and.4.Without.Exceedance <- Percent.Options.3.and.4.Without.Exceedance
    reset.demo()
    ### rbind unique IRW counts then export
    count.unique.IRW <- c(length(NEHCT3.b.exceed.FIPS), length(NEHCT3.without.exceed.FIPS), length(NEHCT3.Opt1and2.exceed.FIPS), length(NEHCT3.Opt1and2.wo.exceed.FIPS), length(NEHCT3.Opt3and4.exceed.FIPS), length(NEHCT3.Opt3and4.wo.exceed.FIPS))
    IRW.wildlife.NEHCT3.Demographics <- rbind(IRW.wildlife.NEHCT3.Demographics, "Count of Unique FIPS" = count.unique.IRW)
    setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023/Task 4")
    write.csv(IRW.wildlife.NEHCT3.Demographics, "IRW.wildlife.NEHCT3.Demographics.csv")
#read in Human Health noncancer RfD sheet and bin COMIDS 
setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023/Task 1")
RfD <- read.csv("Updated CSV - HH_noncancer - IRW Model_EA Results.csv")
  ##baseline exceedances
    rfd.cs.exc <- c() #child subsistence baseline
    rfd.cs.wo.exc <- c() #baseline w/o exceedance
    rfd.cs.o1.exc <- c() #option 1 cs exceed
    rfd.cs.o1.wo.exc <- c()
    rfd.cs.o2.exc <- c()
    rfd.cs.o2.wo.exc <- c()
    rfd.cs.o3.4.exc <- c() #options 3 and 4 cs exceed
    rfd.cs.o3.4.wo.exc <- c()
    
    rfd.as.exc <- c() #adult subsistence baseline
    rfd.as.wo.exc <- c()
    rfd.as.o1.exc <- c() 
    rfd.as.o1.wo.exc <- c()
    rfd.as.o2.exc <- c()
    rfd.as.o2.wo.exc <- c()
    rfd.as.o3.4.exc <- c() 
    rfd.as.o3.4.wo.exc <- c()
    
    rfd.cr.exc <- c() #child recreational
    rfd.cr.wo.exc <- c()
    rfd.cr.o1.exc <- c() 
    rfd.cr.o1.wo.exc <- c()
    rfd.cr.o2.exc <- c()
    rfd.cr.o2.wo.exc <- c()
    rfd.cr.o3.4.exc <- c() 
    rfd.cr.o3.4.wo.exc <- c()
    
    rfd.ar.exc <- c() #adult recreational
    rfd.ar.wo.exc <- c()
    rfd.ar.o1.exc <- c() 
    rfd.ar.o1.wo.exc <- c()
    rfd.ar.o2.exc <- c()
    rfd.ar.o2.wo.exc <- c()
    rfd.ar.o3.4.exc <- c() 
    rfd.ar.o3.4.wo.exc <- c()
  for(COMID in RfD$COMID){
    #Child subsistence
    idx <- which(RfD$COMID == COMID & RfD$Cohort.Group == "Child (subsistence)") #which finds indices in RfD spreadsheet where COMID is and where cohort is cs
      ## Baseline
      if(!(COMID %in% rfd.cs.exc) & (any(RfD$Exceed.Baseline.HQ[idx] == 1))) #determine if COMID is already in exceed vec and if COMID HQ exceeds
        rfd.cs.exc <- c(rfd.cs.exc, COMID) #if exceeds, add to exceed vec
      if(!(COMID %in% rfd.cs.wo.exc) & (!(COMID %in% rfd.cs.exc))) #if is not yet in exceed vec or not exceed vec, add to not exceed vec
        rfd.cs.wo.exc <- c(rfd.cs.wo.exc, COMID)
      ## Option 1
      if(!(COMID %in% rfd.cs.o1.exc) & (any(RfD$Exceed.Option1.HQ[idx] == 1))) 
        rfd.cs.o1.exc <- c(rfd.cs.o1.exc, COMID) 
      if(!(COMID %in% rfd.cs.o1.wo.exc) & (!(COMID %in% rfd.cs.o1.exc))) 
        rfd.cs.o1.wo.exc <- c(rfd.cs.o1.wo.exc, COMID)
      ## Option 2 
      if(!(COMID %in% rfd.cs.o2.exc) & (any(RfD$Exceed.Option2.HQ[idx] == 1))) 
        rfd.cs.o2.exc <- c(rfd.cs.o2.exc, COMID) 
      if(!(COMID %in% rfd.cs.o2.wo.exc) & (!(COMID %in% rfd.cs.o2.exc))) 
        rfd.cs.o2.wo.exc <- c(rfd.cs.o2.wo.exc, COMID)
      ## Options 3 and 4
      if(!(COMID %in% rfd.cs.o3.4.exc) & (any(RfD$Exceed.Option3.HQ[idx] == 1))) # sum of RfD$Exceed.Option3.HQ == sum of RfD$Exceed.Option4.HQ
        rfd.cs.o3.4.exc <- c(rfd.cs.o3.4.exc, COMID) 
      if(!(COMID %in% rfd.cs.o3.4.wo.exc) & (!(COMID %in% rfd.cs.o3.4.exc))) 
        rfd.cs.o3.4.wo.exc <- c(rfd.cs.o3.4.wo.exc, COMID)
    #Adult subsistence
    idx <- which(RfD$COMID == COMID & RfD$Cohort.Group == "Adult (subsistence)")
      ##Baseline
      if(!(COMID %in% rfd.as.exc) & (any(RfD$Exceed.Baseline.HQ[idx] == 1))) 
        rfd.as.exc <- c(rfd.as.exc, COMID)
      if(!(COMID %in% rfd.as.wo.exc) & (!(COMID %in% rfd.as.exc)))
        rfd.as.wo.exc <- c(rfd.as.wo.exc, COMID)
      ##Option 1
      if(!(COMID %in% rfd.as.o1.exc) & (any(RfD$Exceed.Option1.HQ[idx] == 1))) 
        rfd.as.o1.exc <- c(rfd.as.o1.exc, COMID)
      if(!(COMID %in% rfd.as.o1.wo.exc) & (!(COMID %in% rfd.as.o1.exc)))
        rfd.as.o1.wo.exc <- c(rfd.as.o1.wo.exc, COMID)
      ##Option 2
      if(!(COMID %in% rfd.as.o2.exc) & (any(RfD$Exceed.Option2.HQ[idx] == 1))) 
        rfd.as.o2.exc <- c(rfd.as.o2.exc, COMID)
      if(!(COMID %in% rfd.as.o2.wo.exc) & (!(COMID %in% rfd.as.o2.exc)))
        rfd.as.o2.wo.exc <- c(rfd.as.o2.wo.exc, COMID)
      ## Options 3 and 4
      if(!(COMID %in% rfd.as.o3.4.exc) & (any(RfD$Exceed.Option3.HQ[idx] == 1))) 
        rfd.as.o3.4.exc <- c(rfd.as.o3.4.exc, COMID)
      if(!(COMID %in% rfd.as.o3.4.wo.exc) & (!(COMID %in% rfd.as.o3.4.exc)))
        rfd.as.o3.4.wo.exc <- c(rfd.as.o3.4.wo.exc, COMID)
    #Child recreational
    idx <- which(RfD$COMID == COMID & RfD$Cohort.Group == "Child (recreational)")
      ##Baseline
      if(!(COMID %in% rfd.cr.exc) & (any(RfD$Exceed.Baseline.HQ[idx] == 1))) #determine if COMID is already in exceed vec and if COMID HQ exceeds
        rfd.cr.exc <- c(rfd.cr.exc, COMID) #if exceeds, add to exceed vec
      if(!(COMID %in% rfd.cr.wo.exc) & (!(COMID %in% rfd.cr.exc))) #if is not yet in exceed vec or not exceed vec, add to not exceed vec
        rfd.cr.wo.exc <- c(rfd.cr.wo.exc, COMID)
      ##Option 1
      if(!(COMID %in% rfd.cr.o1.exc) & (any(RfD$Exceed.Option1.HQ[idx] == 1))) 
        rfd.cr.o1.exc <- c(rfd.cr.o1.exc, COMID) 
      if(!(COMID %in% rfd.cr.o1.wo.exc) & (!(COMID %in% rfd.cr.o1.exc))) 
        rfd.cr.o1.wo.exc <- c(rfd.cr.o1.wo.exc, COMID)
      ##Option 2  
      if(!(COMID %in% rfd.cr.o2.exc) & (any(RfD$Exceed.Option2.HQ[idx] == 1))) 
        rfd.cr.o2.exc <- c(rfd.cr.o2.exc, COMID) 
      if(!(COMID %in% rfd.cr.o2.wo.exc) & (!(COMID %in% rfd.cr.o2.exc))) 
        rfd.cr.o2.wo.exc <- c(rfd.cr.o2.wo.exc, COMID)
      ##Options 3 and 4
      if(!(COMID %in% rfd.cr.o3.4.exc) & (any(RfD$Exceed.Option3.HQ[idx] == 1))) 
        rfd.cr.o3.4.exc <- c(rfd.cr.o3.4.exc, COMID) 
      if(!(COMID %in% rfd.cr.o3.4.wo.exc) & (!(COMID %in% rfd.cr.o3.4.exc))) 
        rfd.cr.o3.4.wo.exc <- c(rfd.cr.o3.4.wo.exc, COMID)
    #Adult recreational
    idx <- which(RfD$COMID == COMID & RfD$Cohort.Group == "Adult (recreational)")
      ##Baseline
      if(!(COMID %in% rfd.ar.exc) & (any(RfD$Exceed.Baseline.HQ[idx] == 1))) 
        rfd.ar.exc <- c(rfd.ar.exc, COMID)
      if(!(COMID %in% rfd.ar.wo.exc) & (!(COMID %in% rfd.ar.exc)))
        rfd.ar.wo.exc <- c(rfd.ar.wo.exc, COMID)
      ##Option 1 
      if(!(COMID %in% rfd.ar.o1.exc) & (any(RfD$Exceed.Option1.HQ[idx] == 1))) 
        rfd.ar.o1.exc <- c(rfd.ar.o1.exc, COMID)
      if(!(COMID %in% rfd.ar.o1.wo.exc) & (!(COMID %in% rfd.ar.o1.exc)))
        rfd.ar.o1.wo.exc <- c(rfd.ar.o1.wo.exc, COMID)
      ##Option 2
      if(!(COMID %in% rfd.ar.o2.exc) & (any(RfD$Exceed.Option2.HQ[idx] == 1))) 
        rfd.ar.o2.exc <- c(rfd.ar.o2.exc, COMID)
      if(!(COMID %in% rfd.ar.o2.wo.exc) & (!(COMID %in% rfd.ar.o2.exc)))
        rfd.ar.o2.wo.exc <- c(rfd.ar.o2.wo.exc, COMID)
      ##Option 3
      if(!(COMID %in% rfd.ar.o3.4.exc) & (any(RfD$Exceed.Option3.HQ[idx] == 1))) 
        rfd.ar.o3.4.exc <- c(rfd.ar.o3.4.exc, COMID)
      if(!(COMID %in% rfd.ar.o3.4.wo.exc) & (!(COMID %in% rfd.ar.o3.4.exc)))
        rfd.ar.o3.4.wo.exc <- c(rfd.ar.o3.4.wo.exc, COMID)
  }
#Bin populations by exceedance for all 4 RfD cohorts
  ##Adult Recreational
    ###Baseline with exceedance
    rfd.ar.exc.FIPS <- sapply(rfd.ar.exc, steamp.FIPS) %>% unique()
    rfd.ar.exc.demo <- sapply(rfd.ar.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.ar.exc))
    IRW.HH.noncancer.Adult.Recreational.Demo <- data.frame(Percent.Baseline.With.Exceedance)
    reset.demo()
    ###Baseline without exceedance
    rfd.ar.wo.exc.FIPS <- sapply(rfd.ar.wo.exc, steamp.FIPS) %>% unique()
    rfd.ar.wo.exc.demo <- sapply(rfd.ar.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.ar.wo.exc))
    IRW.HH.noncancer.Adult.Recreational.Demo$Percent.Baseline.Without.Exceedance <- Percent.Baseline.Without.Exceedance
    reset.demo()
    ###Option 1 with exceedance
    rfd.ar.o1.exc.FIPS <- sapply(rfd.ar.o1.exc, steamp.FIPS) %>% unique()
    rfd.ar.o1.exc.demo <- sapply(rfd.ar.o1.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.1.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.ar.o1.exc))
    IRW.HH.noncancer.Adult.Recreational.Demo$Percent.Option.1.With.Exceedance <- Percent.Option.1.With.Exceedance
    reset.demo()
    ### Option 1 without exceedance
    rfd.ar.o1.wo.exc.FIPS <- sapply(rfd.ar.o1.wo.exc, steamp.FIPS) %>% unique()
    rfd.ar.o1.wo.exc.demo <- sapply(rfd.ar.o1.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.1.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.ar.o1.wo.exc))
    IRW.HH.noncancer.Adult.Recreational.Demo$Percent.Option.1.Without.Exceedance <- Percent.Option.1.Without.Exceedance
    reset.demo()
    ### Option 2 with exceedance
    rfd.ar.o2.exc.FIPS <- sapply(rfd.ar.o2.exc, steamp.FIPS) %>% unique()
    rfd.ar.o2.exc.demo <- sapply(rfd.ar.o2.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.ar.o2.exc))
    IRW.HH.noncancer.Adult.Recreational.Demo$Percent.Option.2.With.Exceedance <- Percent.Option.2.With.Exceedance
    reset.demo()
    ### Option 2 without exceedance
    rfd.ar.o2.wo.exc.FIPS <- sapply(rfd.ar.o2.wo.exc, steamp.FIPS) %>% unique()
    rfd.ar.o2.wo.exc.demo <- sapply(rfd.ar.o2.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.ar.o2.wo.exc))
    IRW.HH.noncancer.Adult.Recreational.Demo$Percent.Option.2.Without.Exceedance <- Percent.Option.2.Without.Exceedance
    reset.demo()
    ### Options 3 and 4 with exceedance
    rfd.ar.o3.4.exc.FIPS <- sapply(rfd.ar.o3.4.exc, steamp.FIPS) %>% unique()
    rfd.ar.o3.4.exc.demo <- sapply(rfd.ar.o3.4.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.ar.o3.4.exc))
    IRW.HH.noncancer.Adult.Recreational.Demo$Percent.Option.3.and.4.With.Exceedance <- Percent.Option.3.and.4.With.Exceedance
    reset.demo()
    ### Options 3 and 4 without exceedance
    rfd.ar.o3.4.wo.exc.FIPS <- sapply(rfd.ar.o3.4.wo.exc, steamp.FIPS) %>% unique()
    rfd.ar.o3.4.wo.exc.demo <- sapply(rfd.ar.o3.4.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.ar.o3.4.wo.exc))
    IRW.HH.noncancer.Adult.Recreational.Demo$Percent.Option.3.and.4.Without.Exceedance <- Percent.Option.3.and.4.Without.Exceedance
    reset.demo()
    ###rbind unique IRW
    count.unique.IRW <- c(length(rfd.ar.exc.FIPS), length(rfd.ar.wo.exc.FIPS), length(rfd.ar.o1.exc.FIPS), length(rfd.ar.o1.wo.exc.FIPS), length(rfd.ar.o2.exc.FIPS), length(rfd.ar.o2.wo.exc.FIPS), length(rfd.ar.o3.4.exc.FIPS), length(rfd.ar.o3.4.wo.exc.FIPS))
    IRW.HH.noncancer.Adult.Recreational.Demo <- rbind(IRW.HH.noncancer.Adult.Recreational.Demo, "Count of Unique FIPS" = count.unique.IRW)
  ##Adult Subsistence
    ###Baseline with exceedance
    rfd.as.exc.FIPS <- sapply(rfd.as.exc, steamp.FIPS) %>% unique()
    rfd.as.exc.demo <- sapply(rfd.as.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.as.exc))
    IRW.HH.noncancer.Adult.Subsistence.Demo <- data.frame(Percent.Baseline.With.Exceedance)
    reset.demo()
    ###Baseline without exceedance
    rfd.as.wo.exc.FIPS <- sapply(rfd.as.wo.exc, steamp.FIPS) %>% unique()
    rfd.as.wo.exc.demo <- sapply(rfd.as.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.as.wo.exc))
    IRW.HH.noncancer.Adult.Subsistence.Demo$Percent.Baseline.Without.Exceedance <- Percent.Baseline.Without.Exceedance
    reset.demo()
    ###Option 1 with exceedance
    rfd.as.o1.exc.FIPS <- sapply(rfd.as.o1.exc, steamp.FIPS) %>% unique()
    rfd.as.o1.exc.demo <- sapply(rfd.as.o1.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.1.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.as.o1.exc))
    IRW.HH.noncancer.Adult.Subsistence.Demo$Percent.Option.1.With.Exceedance <- Percent.Option.1.With.Exceedance
    reset.demo()
    ### Option 1 without exceedance
    rfd.as.o1.wo.exc.FIPS <- sapply(rfd.as.o1.wo.exc, steamp.FIPS) %>% unique()
    rfd.as.o1.wo.exc.demo <- sapply(rfd.as.o1.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.1.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.as.o1.wo.exc))
    IRW.HH.noncancer.Adult.Subsistence.Demo$Percent.Option.1.Without.Exceedance <- Percent.Option.1.Without.Exceedance
    reset.demo()
    ### Option 2 with exceedance
    rfd.as.o2.exc.FIPS <- sapply(rfd.as.o2.exc, steamp.FIPS) %>% unique()
    rfd.as.o2.exc.demo <- sapply(rfd.as.o2.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.as.o2.exc))
    IRW.HH.noncancer.Adult.Subsistence.Demo$Percent.Option.2.With.Exceedance <- Percent.Option.2.With.Exceedance
    reset.demo()
    ### Option 2 without exceedance
    rfd.as.o2.wo.exc.FIPS <- sapply(rfd.as.o2.wo.exc, steamp.FIPS) %>% unique()
    rfd.as.o2.wo.exc.demo <- sapply(rfd.as.o2.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.as.o2.wo.exc))
    IRW.HH.noncancer.Adult.Subsistence.Demo$Percent.Option.2.Without.Exceedance <- Percent.Option.2.Without.Exceedance
    reset.demo()
    ### Options 3 and 4 with exceedance
    rfd.as.o3.4.exc.FIPS <- sapply(rfd.as.o3.4.exc, steamp.FIPS) %>% unique()
    rfd.as.o3.4.exc.demo <- sapply(rfd.as.o3.4.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.as.o3.4.exc))
    IRW.HH.noncancer.Adult.Subsistence.Demo$Percent.Option.3.and.4.With.Exceedance <- Percent.Option.3.and.4.With.Exceedance
    reset.demo()
    ### Options 3 and 4 without exceedance
    rfd.as.o3.4.wo.exc.FIPS <- sapply(rfd.as.o3.4.wo.exc, steamp.FIPS) %>% unique()
    rfd.as.o3.4.wo.exc.demo <- sapply(rfd.as.o3.4.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.as.o3.4.wo.exc))
    IRW.HH.noncancer.Adult.Subsistence.Demo$Percent.Option.3.and.4.Without.Exceedance <- Percent.Option.3.and.4.Without.Exceedance
    reset.demo()
    ###rbind unique IRW
    count.unique.IRW <- c(length(rfd.as.exc.FIPS), length(rfd.as.wo.exc.FIPS), length(rfd.as.o1.exc.FIPS), length(rfd.as.o1.wo.exc.FIPS), length(rfd.as.o2.exc.FIPS), length(rfd.as.o2.wo.exc.FIPS), length(rfd.as.o3.4.exc.FIPS), length(rfd.as.o3.4.wo.exc.FIPS))
    IRW.HH.noncancer.Adult.Subsistence.Demo <- rbind(IRW.HH.noncancer.Adult.Subsistence.Demo, "Count of Unique FIPS" = count.unique.IRW)
    
   ##Child Recreational
    ###Baseline with exceedance
    rfd.cr.exc.FIPS <- sapply(rfd.cr.exc, steamp.FIPS) %>% unique()
    rfd.cr.exc.demo <- sapply(rfd.cr.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cr.exc))
    IRW.HH.noncancer.Child.Recreational.Demo <- data.frame(Percent.Baseline.With.Exceedance)
    reset.demo()
    ###Baseline without exceedance
    rfd.cr.wo.exc.FIPS <- sapply(rfd.cr.wo.exc, steamp.FIPS) %>% unique()
    rfd.cr.wo.exc.demo <- sapply(rfd.cr.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cr.wo.exc))
    IRW.HH.noncancer.Child.Recreational.Demo$Percent.Baseline.Without.Exceedance <- Percent.Baseline.Without.Exceedance
    reset.demo()
    ###Option 1 with exceedance
    rfd.cr.o1.exc.FIPS <- sapply(rfd.cr.o1.exc, steamp.FIPS) %>% unique()
    rfd.cr.o1.exc.demo <- sapply(rfd.cr.o1.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.1.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cr.o1.exc))
    IRW.HH.noncancer.Child.Recreational.Demo$Percent.Option.1.With.Exceedance <- Percent.Option.1.With.Exceedance
    reset.demo()
    ### Option 1 without exceedance
    rfd.cr.o1.wo.exc.FIPS <- sapply(rfd.cr.o1.wo.exc, steamp.FIPS) %>% unique()
    rfd.cr.o1.wo.exc.demo <- sapply(rfd.cr.o1.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.1.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cr.o1.wo.exc))
    IRW.HH.noncancer.Child.Recreational.Demo$Percent.Option.1.Without.Exceedance <- Percent.Option.1.Without.Exceedance
    reset.demo()
    ### Option 2 with exceedance
    rfd.cr.o2.exc.FIPS <- sapply(rfd.cr.o2.exc, steamp.FIPS) %>% unique()
    rfd.cr.o2.exc.demo <- sapply(rfd.cr.o2.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cr.o2.exc))
    IRW.HH.noncancer.Child.Recreational.Demo$Percent.Option.2.With.Exceedance <- Percent.Option.2.With.Exceedance
    reset.demo()
    ### Option 2 without exceedance
    rfd.cr.o2.wo.exc.FIPS <- sapply(rfd.cr.o2.wo.exc, steamp.FIPS) %>% unique()
    rfd.cr.o2.wo.exc.demo <- sapply(rfd.cr.o2.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cr.o2.wo.exc))
    IRW.HH.noncancer.Child.Recreational.Demo$Percent.Option.2.Without.Exceedance <- Percent.Option.2.Without.Exceedance
    reset.demo()
    ### Options 3 and 4 with exceedance
    rfd.cr.o3.4.exc.FIPS <- sapply(rfd.cr.o3.4.exc, steamp.FIPS) %>% unique()
    rfd.cr.o3.4.exc.demo <- sapply(rfd.cr.o3.4.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cr.o3.4.exc))
    IRW.HH.noncancer.Child.Recreational.Demo$Percent.Option.3.and.4.With.Exceedance <- Percent.Option.3.and.4.With.Exceedance
    reset.demo()
    ### Options 3 and 4 without exceedance
    rfd.cr.o3.4.wo.exc.FIPS <- sapply(rfd.cr.o3.4.wo.exc, steamp.FIPS) %>% unique()
    rfd.cr.o3.4.wo.exc.demo <- sapply(rfd.cr.o3.4.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cr.o3.4.wo.exc))
    IRW.HH.noncancer.Child.Recreational.Demo$Percent.Option.3.and.4.Without.Exceedance <- Percent.Option.3.and.4.Without.Exceedance
    reset.demo()
    ###rbind unique IRW
    count.unique.IRW <- c(length(rfd.cr.exc.FIPS), length(rfd.cr.wo.exc.FIPS), length(rfd.cr.o1.exc.FIPS), length(rfd.cr.o1.wo.exc.FIPS), length(rfd.cr.o2.exc.FIPS), length(rfd.cr.o2.wo.exc.FIPS), length(rfd.cr.o3.4.exc.FIPS), length(rfd.cr.o3.4.wo.exc.FIPS))
    IRW.HH.noncancer.Child.Recreational.Demo <- rbind(IRW.HH.noncancer.Child.Recreational.Demo, "Count of Unique FIPS" = count.unique.IRW)
  ##Child Subsistence
    ###Baseline with exceedance
    rfd.cs.exc.FIPS <- sapply(rfd.cs.exc, steamp.FIPS) %>% unique()
    rfd.cs.exc.demo <- sapply(rfd.cs.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cs.exc))
    IRW.HH.noncancer.Child.Subsistence.Demo <- data.frame(Percent.Baseline.With.Exceedance)
    reset.demo()
    ###Baseline without exceedance
    rfd.cs.wo.exc.FIPS <- sapply(rfd.cs.wo.exc, steamp.FIPS) %>% unique()
    rfd.cs.wo.exc.demo <- sapply(rfd.cs.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cs.wo.exc))
    IRW.HH.noncancer.Child.Subsistence.Demo$Percent.Baseline.Without.Exceedance <- Percent.Baseline.Without.Exceedance
    reset.demo()
    ###Option 1 with exceedance
    rfd.cs.o1.exc.FIPS <- sapply(rfd.cs.o1.exc, steamp.FIPS) %>% unique()
    rfd.cs.o1.exc.demo <- sapply(rfd.cs.o1.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.1.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cs.o1.exc))
    IRW.HH.noncancer.Child.Subsistence.Demo$Percent.Option.1.With.Exceedance <- Percent.Option.1.With.Exceedance
    reset.demo()
    ### Option 1 without exceedance
    rfd.cs.o1.wo.exc.FIPS <- sapply(rfd.cs.o1.wo.exc, steamp.FIPS) %>% unique()
    rfd.cs.o1.wo.exc.demo <- sapply(rfd.cs.o1.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.1.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cs.o1.wo.exc))
    IRW.HH.noncancer.Child.Subsistence.Demo$Percent.Option.1.Without.Exceedance <- Percent.Option.1.Without.Exceedance
    reset.demo()
    ### Option 2 with exceedance
    rfd.cs.o2.exc.FIPS <- sapply(rfd.cs.o2.exc, steamp.FIPS) %>% unique()
    rfd.cs.o2.exc.demo <- sapply(rfd.cs.o2.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cs.o2.exc))
    IRW.HH.noncancer.Child.Subsistence.Demo$Percent.Option.2.With.Exceedance <- Percent.Option.2.With.Exceedance
    reset.demo()
    ### Option 2 without exceedance
    rfd.cs.o2.wo.exc.FIPS <- sapply(rfd.cs.o2.wo.exc, steamp.FIPS) %>% unique()
    rfd.cs.o2.wo.exc.demo <- sapply(rfd.cs.o2.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cs.o2.wo.exc))
    IRW.HH.noncancer.Child.Subsistence.Demo$Percent.Option.2.Without.Exceedance <- Percent.Option.2.Without.Exceedance
    reset.demo()
    ### Options 3 and 4 with exceedance
    rfd.cs.o3.4.exc.FIPS <- sapply(rfd.cs.o3.4.exc, steamp.FIPS) %>% unique()
    rfd.cs.o3.4.exc.demo <- sapply(rfd.cs.o3.4.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cs.o3.4.exc))
    IRW.HH.noncancer.Child.Subsistence.Demo$Percent.Option.3.and.4.With.Exceedance <- Percent.Option.3.and.4.With.Exceedance
    reset.demo()
    ### Options 3 and 4 without exceedance
    rfd.cs.o3.4.wo.exc.FIPS <- sapply(rfd.cs.o3.4.wo.exc, steamp.FIPS) %>% unique()
    rfd.cs.o3.4.wo.exc.demo <- sapply(rfd.cs.o3.4.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Option.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(rfd.cs.o3.4.wo.exc))
    IRW.HH.noncancer.Child.Subsistence.Demo$Percent.Option.3.and.4.Without.Exceedance <- Percent.Option.3.and.4.Without.Exceedance
    reset.demo()
    ###rbind unique IRW
    count.unique.IRW <- c(length(rfd.cs.exc.FIPS), length(rfd.cs.wo.exc.FIPS), length(rfd.cs.o1.exc.FIPS), length(rfd.cs.o1.wo.exc.FIPS), length(rfd.cs.o2.exc.FIPS), length(rfd.cs.o2.wo.exc.FIPS), length(rfd.cs.o3.4.exc.FIPS), length(rfd.cs.o3.4.wo.exc.FIPS))
    IRW.HH.noncancer.Child.Subsistence.Demo <- rbind(IRW.HH.noncancer.Child.Subsistence.Demo, "Count of Unique FIPS" = count.unique.IRW)
    setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023/Task 4")
    write.csv(IRW.HH.noncancer.Adult.Recreational.Demo, "IRW.HH.noncancer.Adult.Recreational.Demo.csv")
    write.csv(IRW.HH.noncancer.Adult.Subsistence.Demo, "IRW.HH.noncancer.Adult.Subsistence.Demo.csv")
    write.csv(IRW.HH.noncancer.Child.Recreational.Demo, "IRW.HH.noncancer.Child.Recreational.Demo.csv")
    write.csv(IRW.HH.noncancer.Child.Subsistence.Demo, "IRW.HH.noncancer.Child.Subsistence.Demo.csv")
#read in Human Health Cancer LECR sheet and bin COMIDS by exceedance
setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023/Task 1")
LECR <- read.csv("Updated CSV - HH_cancer - IRW Model_EA Results.csv")
LECR.cs.exc <- c() #child subsistence baseline
LECR.cs.wo.exc <- c() #baseline w/o exceedance
LECR.cs.o1.exc <- c() #option 1 cs exceed
LECR.cs.o1.wo.exc <- c()
LECR.cs.o2.exc <- c()
LECR.cs.o2.wo.exc <- c()
LECR.cs.o3.4.exc <- c() #options 3 and 4 cs exceed
LECR.cs.o3.4.wo.exc <- c()

LECR.as.exc <- c() #adult subsistence baseline
LECR.as.wo.exc <- c()
LECR.as.o1.exc <- c() 
LECR.as.o1.wo.exc <- c()
LECR.as.o2.exc <- c()
LECR.as.o2.wo.exc <- c()
LECR.as.o3.4.exc <- c() 
LECR.as.o3.4.wo.exc <- c()

LECR.cr.exc <- c() #child recreational
LECR.cr.wo.exc <- c()
LECR.cr.o1.exc <- c() 
LECR.cr.o1.wo.exc <- c()
LECR.cr.o2.exc <- c()
LECR.cr.o2.wo.exc <- c()
LECR.cr.o3.4.exc <- c() 
LECR.cr.o3.4.wo.exc <- c()

LECR.ar.exc <- c() #adult recreational
LECR.ar.wo.exc <- c()
LECR.ar.o1.exc <- c() 
LECR.ar.o1.wo.exc <- c()
LECR.ar.o2.exc <- c()
LECR.ar.o2.wo.exc <- c()
LECR.ar.o3.4.exc <- c() 
LECR.ar.o3.4.wo.exc <- c()
for(COMID in LECR$COMID){
  #Child subsistence
  idx <- which(LECR$COMID == COMID & LECR$Cohort.Group == "Child (subsistence)") #which finds indices in LECR spreadsheet where COMID is and where cohort is cs
  ## Baseline
  if(!(COMID %in% LECR.cs.exc) & (any(LECR$Exceed.Baseline.LECR[idx] == 1))) #determine if COMID is already in exceed vec and if COMID HQ exceeds
    LECR.cs.exc <- c(LECR.cs.exc, COMID) #if exceeds, add to exceed vec
  if(!(COMID %in% LECR.cs.wo.exc) & (!(COMID %in% LECR.cs.exc))) #if is not yet in exceed vec or not exceed vec, add to not exceed vec
    LECR.cs.wo.exc <- c(LECR.cs.wo.exc, COMID)
  ## Options 1 and 2
  if(!(COMID %in% LECR.cs.o1.exc) & (any(LECR$Exceed.Option1.LECR[idx] == 1))) #sum of LECR$Exceed.Option3.LECR == sum of LECR$Exceed.Option4.HQ
    LECR.cs.o1.exc <- c(LECR.cs.o1.exc, COMID) 
  if(!(COMID %in% LECR.cs.o1.wo.exc) & (!(COMID %in% LECR.cs.o1.exc))) 
    LECR.cs.o1.wo.exc <- c(LECR.cs.o1.wo.exc, COMID)
  ## Options 3 and 4
  if(!(COMID %in% LECR.cs.o3.4.exc) & (any(LECR$Exceed.Option3.LECR[idx] == 1))) # sum of LECR$Exceed.Option3.LECR == sum of LECR$Exceed.Option4.HQ
    LECR.cs.o3.4.exc <- c(LECR.cs.o3.4.exc, COMID) 
  if(!(COMID %in% LECR.cs.o3.4.wo.exc) & (!(COMID %in% LECR.cs.o3.4.exc))) 
    LECR.cs.o3.4.wo.exc <- c(LECR.cs.o3.4.wo.exc, COMID)
  #Adult subsistence
  idx <- which(LECR$COMID == COMID & LECR$Cohort.Group == "Adult (subsistence)")
  ##Baseline
  if(!(COMID %in% LECR.as.exc) & (any(LECR$Exceed.Baseline.LECR[idx] == 1))) 
    LECR.as.exc <- c(LECR.as.exc, COMID)
  if(!(COMID %in% LECR.as.wo.exc) & (!(COMID %in% LECR.as.exc)))
    LECR.as.wo.exc <- c(LECR.as.wo.exc, COMID)
  ##Options 1 and 2
  if(!(COMID %in% LECR.as.o1.exc) & (any(LECR$Exceed.Option1.LECR[idx] == 1))) 
    LECR.as.o1.exc <- c(LECR.as.o1.exc, COMID)
  if(!(COMID %in% LECR.as.o1.wo.exc) & (!(COMID %in% LECR.as.o1.exc)))
    LECR.as.o1.wo.exc <- c(LECR.as.o1.wo.exc, COMID)
  ## Options 3 and 4
  if(!(COMID %in% LECR.as.o3.4.exc) & (any(LECR$Exceed.Option3.LECR[idx] == 1))) 
    LECR.as.o3.4.exc <- c(LECR.as.o3.4.exc, COMID)
  if(!(COMID %in% LECR.as.o3.4.wo.exc) & (!(COMID %in% LECR.as.o3.4.exc)))
    LECR.as.o3.4.wo.exc <- c(LECR.as.o3.4.wo.exc, COMID)
  #Child recreational
  idx <- which(LECR$COMID == COMID & LECR$Cohort.Group == "Child (recreational)")
  ##Baseline
  if(!(COMID %in% LECR.cr.exc) & (any(LECR$Exceed.Baseline.LECR[idx] == 1))) 
    LECR.cr.exc <- c(LECR.cr.exc, COMID) #if exceeds, add to exceed vec
  if(!(COMID %in% LECR.cr.wo.exc) & (!(COMID %in% LECR.cr.exc))) 
    LECR.cr.wo.exc <- c(LECR.cr.wo.exc, COMID)
  ##Options 1 and 2
  if(!(COMID %in% LECR.cr.o1.exc) & (any(LECR$Exceed.Option1.LECR[idx] == 1))) 
    LECR.cr.o1.exc <- c(LECR.cr.o1.exc, COMID) 
  if(!(COMID %in% LECR.cr.o1.wo.exc) & (!(COMID %in% LECR.cr.o1.exc))) 
    LECR.cr.o1.wo.exc <- c(LECR.cr.o1.wo.exc, COMID)
  ##Options 3 and 4
  if(!(COMID %in% LECR.cr.o3.4.exc) & (any(LECR$Exceed.Option3.LECR[idx] == 1))) 
    LECR.cr.o3.4.exc <- c(LECR.cr.o3.4.exc, COMID) 
  if(!(COMID %in% LECR.cr.o3.4.wo.exc) & (!(COMID %in% LECR.cr.o3.4.exc))) 
    LECR.cr.o3.4.wo.exc <- c(LECR.cr.o3.4.wo.exc, COMID)
  #Adult recreational
  idx <- which(LECR$COMID == COMID & LECR$Cohort.Group == "Adult (recreational)")
  ##Baseline
  if(!(COMID %in% LECR.ar.exc) & (any(LECR$Exceed.Baseline.LECR[idx] == 1))) 
    LECR.ar.exc <- c(LECR.ar.exc, COMID)
  if(!(COMID %in% LECR.ar.wo.exc) & (!(COMID %in% LECR.ar.exc)))
    LECR.ar.wo.exc <- c(LECR.ar.wo.exc, COMID)
  ##Options 1 and 2
  if(!(COMID %in% LECR.ar.o1.exc) & (any(LECR$Exceed.Option1.LECR[idx] == 1))) 
    LECR.ar.o1.exc <- c(LECR.ar.o1.exc, COMID)
  if(!(COMID %in% LECR.ar.o1.wo.exc) & (!(COMID %in% LECR.ar.o1.exc)))
    LECR.ar.o1.wo.exc <- c(LECR.ar.o1.wo.exc, COMID)
  ##Options 3 and 4
  if(!(COMID %in% LECR.ar.o3.4.exc) & (any(LECR$Exceed.Option3.LECR[idx] == 1))) 
    LECR.ar.o3.4.exc <- c(LECR.ar.o3.4.exc, COMID)
  if(!(COMID %in% LECR.ar.o3.4.wo.exc) & (!(COMID %in% LECR.ar.o3.4.exc)))
    LECR.ar.o3.4.wo.exc <- c(LECR.ar.o3.4.wo.exc, COMID)
}

# Bin populations by exceedance
  ##Child Recreational
    ###Baseline with exceedance
    LECR.cr.exc.FIPS <- sapply(LECR.cr.exc, steamp.FIPS) %>% unique()
    LECR.cr.exc.demo <- sapply(LECR.cr.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cr.exc))
    IRW.HH.cancer.Child.Recreational.Demo <- data.frame(Percent.Baseline.With.Exceedance)
    reset.demo()
    ### Baseline without exceedance
    LECR.cr.wo.exc.FIPS <- sapply(LECR.cr.wo.exc, steamp.FIPS) %>% unique()
    LECR.cr.wo.exc.demo <- sapply(LECR.cr.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cr.wo.exc))
    IRW.HH.cancer.Child.Recreational.Demo$Percent.Baseline.Without.Exceedance <- Percent.Baseline.Without.Exceedance
    reset.demo()
    ### Options 1 and 2 with exceedance
    LECR.cr.o1.exc.FIPS <- sapply(LECR.cr.o1.exc, steamp.FIPS) %>% unique()
    LECR.cr.o1.exc.demo <- sapply(LECR.cr.o1.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cr.o1.exc))
    IRW.HH.cancer.Child.Recreational.Demo$Percent.Options.1.and.2.With.Exceedance <- Percent.Options.1.and.2.With.Exceedance
    reset.demo()
    ### Options 1 and 2 without exceedance
    LECR.cr.o1.wo.exc.FIPS <- sapply(LECR.cr.o1.wo.exc, steamp.FIPS) %>% unique()
    LECR.cr.o1.wo.exc.demo <- sapply(LECR.cr.o1.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cr.o1.wo.exc))
    IRW.HH.cancer.Child.Recreational.Demo$Percent.Options.1.and.2.Without.Exceedance <- Percent.Options.1.and.2.Without.Exceedance
    reset.demo()
    ### Options 3 and 4 with exceedance
    LECR.cr.o3.4.exc.FIPS <- sapply(LECR.cr.o3.4.exc, steamp.FIPS) %>% unique()
    LECR.cr.o3.4.exc.demo <- sapply(LECR.cr.o3.4.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cr.o3.4.exc))
    IRW.HH.cancer.Child.Recreational.Demo$Percent.Options.3.and.4.With.Exceedance <- Percent.Options.3.and.4.With.Exceedance
    reset.demo()
    ### Options 3 and 4 without exceedance
    LECR.cr.o3.4.wo.exc.FIPS <- sapply(LECR.cr.o3.4.wo.exc, steamp.FIPS) %>% unique()
    LECR.cr.o3.4.wo.exc.demo <- sapply(LECR.cr.o3.4.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cr.o3.4.wo.exc))
    IRW.HH.cancer.Child.Recreational.Demo$Percent.Options.3.and.4.Without.Exceedance <- Percent.Options.3.and.4.Without.Exceedance
    reset.demo()
    ### rbind unique IRW
    IRW.HH.cancer.Child.Recreational.Demo <- rbind(IRW.HH.cancer.Child.Recreational.Demo, "Count of Unique FIPS" = c(length(LECR.cr.exc.FIPS), length(LECR.cr.wo.exc.FIPS), length(LECR.cr.o1.exc.FIPS), length(LECR.cr.o1.wo.exc.FIPS),length(LECR.cr.o3.4.exc.FIPS), length(LECR.cr.o3.4.wo.exc.FIPS)))
  ##Child Subsistence
    ###Baseline with exceedance
    LECR.cs.exc.FIPS <- sapply(LECR.cs.exc, steamp.FIPS) %>% unique()
    LECR.cs.exc.demo <- sapply(LECR.cs.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cs.exc))
    IRW.HH.cancer.Child.Subsistence.Demo <- data.frame(Percent.Baseline.With.Exceedance)
    reset.demo()
    ### Baseline without exceedance
    LECR.cs.wo.exc.FIPS <- sapply(LECR.cs.wo.exc, steamp.FIPS) %>% unique()
    LECR.cs.wo.exc.demo <- sapply(LECR.cs.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cs.wo.exc))
    IRW.HH.cancer.Child.Subsistence.Demo$Percent.Baseline.Without.Exceedance <- Percent.Baseline.Without.Exceedance
    reset.demo()
    ### Options 1 and 2 with exceedance
    LECR.cs.o1.exc.FIPS <- sapply(LECR.cs.o1.exc, steamp.FIPS) %>% unique()
    LECR.cs.o1.exc.demo <- sapply(LECR.cs.o1.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cs.o1.exc))
    IRW.HH.cancer.Child.Subsistence.Demo$Percent.Options.1.and.2.With.Exceedance <- Percent.Options.1.and.2.With.Exceedance
    reset.demo()
    ### Options 1 and 2 without exceedance
    LECR.cs.o1.wo.exc.FIPS <- sapply(LECR.cs.o1.wo.exc, steamp.FIPS) %>% unique()
    LECR.cs.o1.wo.exc.demo <- sapply(LECR.cs.o1.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cs.o1.wo.exc))
    IRW.HH.cancer.Child.Subsistence.Demo$Percent.Options.1.and.2.Without.Exceedance <- Percent.Options.1.and.2.Without.Exceedance
    reset.demo()
    ### Options 3 and 4 with exceedance
    LECR.cs.o3.4.exc.FIPS <- sapply(LECR.cs.o3.4.exc, steamp.FIPS) %>% unique()
    LECR.cs.o3.4.exc.demo <- sapply(LECR.cs.o3.4.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cs.o3.4.exc))
    IRW.HH.cancer.Child.Subsistence.Demo$Percent.Options.3.and.4.With.Exceedance <- Percent.Options.3.and.4.With.Exceedance
    reset.demo()
    ### Options 3 and 4 without exceedance
    LECR.cs.o3.4.wo.exc.FIPS <- sapply(LECR.cs.o3.4.wo.exc, steamp.FIPS) %>% unique()
    LECR.cs.o3.4.wo.exc.demo <- sapply(LECR.cs.o3.4.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.cs.o3.4.wo.exc))
    IRW.HH.cancer.Child.Subsistence.Demo$Percent.Options.3.and.4.Without.Exceedance <- Percent.Options.3.and.4.Without.Exceedance
    reset.demo()
    ### rbind unique IRW
    IRW.HH.cancer.Child.Subsistence.Demo <- rbind(IRW.HH.cancer.Child.Subsistence.Demo, "Count of Unique FIPS" = c(length(LECR.cs.exc.FIPS), length(LECR.cs.wo.exc.FIPS), length(LECR.cs.o1.exc.FIPS), length(LECR.cs.o1.wo.exc.FIPS),length(LECR.cs.o3.4.exc.FIPS), length(LECR.cs.o3.4.wo.exc.FIPS)))
  ##Adult Subsistence
    ###Baseline with exceedance
    LECR.as.exc.FIPS <- sapply(LECR.as.exc, steamp.FIPS) %>% unique()
    LECR.as.exc.demo <- sapply(LECR.as.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.as.exc))
    IRW.HH.cancer.Adult.Subsistence.Demo <- data.frame(Percent.Baseline.With.Exceedance)
    reset.demo()
    ### Baseline without exceedance
    LECR.as.wo.exc.FIPS <- sapply(LECR.as.wo.exc, steamp.FIPS) %>% unique()
    LECR.as.wo.exc.demo <- sapply(LECR.as.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.as.wo.exc))
    IRW.HH.cancer.Adult.Subsistence.Demo$Percent.Baseline.Without.Exceedance <- Percent.Baseline.Without.Exceedance
    reset.demo()
    ### Options 1 and 2 with exceedance
    LECR.as.o1.exc.FIPS <- sapply(LECR.as.o1.exc, steamp.FIPS) %>% unique()
    LECR.as.o1.exc.demo <- sapply(LECR.as.o1.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.as.o1.exc))
    IRW.HH.cancer.Adult.Subsistence.Demo$Percent.Options.1.and.2.With.Exceedance <- Percent.Options.1.and.2.With.Exceedance
    reset.demo()
    ### Options 1 and 2 without exceedance
    LECR.as.o1.wo.exc.FIPS <- sapply(LECR.as.o1.wo.exc, steamp.FIPS) %>% unique()
    LECR.as.o1.wo.exc.demo <- sapply(LECR.as.o1.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.as.o1.wo.exc))
    IRW.HH.cancer.Adult.Subsistence.Demo$Percent.Options.1.and.2.Without.Exceedance <- Percent.Options.1.and.2.Without.Exceedance
    reset.demo()
    ### Options 3 and 4 with exceedance
    LECR.as.o3.4.exc.FIPS <- sapply(LECR.as.o3.4.exc, steamp.FIPS) %>% unique()
    LECR.as.o3.4.exc.demo <- sapply(LECR.as.o3.4.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.as.o3.4.exc))
    IRW.HH.cancer.Adult.Subsistence.Demo$Percent.Options.3.and.4.With.Exceedance <- Percent.Options.3.and.4.With.Exceedance
    reset.demo()
    ### Options 3 and 4 without exceedance
    LECR.as.o3.4.wo.exc.FIPS <- sapply(LECR.as.o3.4.wo.exc, steamp.FIPS) %>% unique()
    LECR.as.o3.4.wo.exc.demo <- sapply(LECR.as.o3.4.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.as.o3.4.wo.exc))
    IRW.HH.cancer.Adult.Subsistence.Demo$Percent.Options.3.and.4.Without.Exceedance <- Percent.Options.3.and.4.Without.Exceedance
    reset.demo()
    ### rbind unique IRW
    IRW.HH.cancer.Adult.Subsistence.Demo <- rbind(IRW.HH.cancer.Adult.Subsistence.Demo, "Count of Unique FIPS" = c(length(LECR.as.exc.FIPS), length(LECR.as.wo.exc.FIPS), length(LECR.as.o1.exc.FIPS), length(LECR.as.o1.wo.exc.FIPS),length(LECR.as.o3.4.exc.FIPS), length(LECR.as.o3.4.wo.exc.FIPS)))
  ##Adult Recreational
    ###Baseline with exceedance
    LECR.ar.exc.FIPS <- sapply(LECR.ar.exc, steamp.FIPS) %>% unique()
    LECR.ar.exc.demo <- sapply(LECR.ar.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.ar.exc))
    IRW.HH.cancer.Adult.Recreational.Demo <- data.frame(Percent.Baseline.With.Exceedance)
    reset.demo()
    ### Baseline without exceedance
    LECR.ar.wo.exc.FIPS <- sapply(LECR.ar.wo.exc, steamp.FIPS) %>% unique()
    LECR.ar.wo.exc.demo <- sapply(LECR.ar.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Baseline.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.ar.wo.exc))
    IRW.HH.cancer.Adult.Recreational.Demo$Percent.Baseline.Without.Exceedance <- Percent.Baseline.Without.Exceedance
    reset.demo()
    ### Options 1 and 2 with exceedance
    LECR.ar.o1.exc.FIPS <- sapply(LECR.ar.o1.exc, steamp.FIPS) %>% unique()
    LECR.ar.o1.exc.demo <- sapply(LECR.ar.o1.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.ar.o1.exc))
    IRW.HH.cancer.Adult.Recreational.Demo$Percent.Options.1.and.2.With.Exceedance <- Percent.Options.1.and.2.With.Exceedance
    reset.demo()
    ### Options 1 and 2 without exceedance
    LECR.ar.o1.wo.exc.FIPS <- sapply(LECR.ar.o1.wo.exc, steamp.FIPS) %>% unique()
    LECR.ar.o1.wo.exc.demo <- sapply(LECR.ar.o1.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.1.and.2.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.ar.o1.wo.exc))
    IRW.HH.cancer.Adult.Recreational.Demo$Percent.Options.1.and.2.Without.Exceedance <- Percent.Options.1.and.2.Without.Exceedance
    reset.demo()
    ### Options 3 and 4 with exceedance
    LECR.ar.o3.4.exc.FIPS <- sapply(LECR.ar.o3.4.exc, steamp.FIPS) %>% unique()
    LECR.ar.o3.4.exc.demo <- sapply(LECR.ar.o3.4.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.With.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.ar.o3.4.exc))
    IRW.HH.cancer.Adult.Recreational.Demo$Percent.Options.3.and.4.With.Exceedance <- Percent.Options.3.and.4.With.Exceedance
    reset.demo()
    ### Options 3 and 4 without exceedance
    LECR.ar.o3.4.wo.exc.FIPS <- sapply(LECR.ar.o3.4.wo.exc, steamp.FIPS) %>% unique()
    LECR.ar.o3.4.wo.exc.demo <- sapply(LECR.ar.o3.4.wo.exc.FIPS, FIPS.demo.sum)
    percent.demo()
    Percent.Options.3.and.4.Without.Exceedance <- c("American Indian" = round(American.Indian.per * 100, digits = 2), "Asian" = round(Asian.per * 100, digits = 2), "African American" = round(Black.per * 100, digits = 2), "Latino" = round(Latino.per * 100, digits = 2), "Other" = round(Other.per * 100, digits = 2), "White" = round(White.per * 100, digits = 2), "Total" = Total, "Count of IRW" = length(LECR.ar.o3.4.wo.exc))
    IRW.HH.cancer.Adult.Recreational.Demo$Percent.Options.3.and.4.Without.Exceedance <- Percent.Options.3.and.4.Without.Exceedance
    reset.demo()
    ### rbind unique IRW
    IRW.HH.cancer.Adult.Recreational.Demo <- rbind(IRW.HH.cancer.Adult.Recreational.Demo, "Count of Unique FIPS" = c(length(LECR.ar.exc.FIPS), length(LECR.ar.wo.exc.FIPS), length(LECR.ar.o1.exc.FIPS), length(LECR.ar.o1.wo.exc.FIPS),length(LECR.ar.o3.4.exc.FIPS), length(LECR.ar.o3.4.wo.exc.FIPS)))
  ## Write tables to csv
    setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023/Task 4")
    write.csv(IRW.HH.cancer.Adult.Recreational.Demo, "IRW.HH.cancer.Adult.Recreational.Demo.csv")
    write.csv(IRW.HH.cancer.Adult.Subsistence.Demo, "IRW.HH.cancer.Adult.Subsistence.Demo.csv")
    write.csv(IRW.HH.cancer.Child.Recreational.Demo, "IRW.HH.cancer.Child.Recreational.Demo.csv")
    write.csv(IRW.HH.cancer.Child.Subsistence.Demo, "IRW.HH.cancer.Child.Subsistence.Demo.csv")
    