## ---------------------------
##
## Script name: Hybrid Species Distribution Model
##
## Purpose of script: This function uses the equations developed with Patrick
## Lambert to estimate spawner abundance for each catchment i at time t
##
## Author: Dr. Betsy Barber
##
## Date Created: 2020-09-21
## Date Updated: 2020-12-02
##
## Copyright (c) Betsy Barber,
## Email: betsy.barber@maine.edu, betsy.barber@inrae.fr
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
## ---------------------------
##
## Notes: Must specify the following dataframes: 
##
## 1) Projected environmental predictor variables from 1951-2100
## 2) Historical observed environmental predictor variables as mean values from 1901-1911
## 3) Catchment-specific information from EuroDiad v.4
## 4) Distance matrix storing all pairwise distances between catchments in the Atlantic Area
## 5) Species-specific parameters (as weighted means) from expert survey
## 6) Species-specific metaparameters from calibrated brtModel (simplified model)

## ---------------------------

## Clear the workspace:
#rm(list = ls())

#### Section 1: Load the packages we will need:  ----

require(tidyverse)
require(gbm)
library(xlsx)

#### Section 2: Load the data files that will be needed. ---------
### set the working directory:
#setwd("C:/Users/betsy.barber/Work Folders/Documents/HDSM/HSDM_Script_Full/HDSM_all_files/HSDM")

### 2.1. Environmental predictor variables:
## Climate data from three climate models - df is projected monthly values from 1951-2100
# WILL NEED TO UPDATE THESE DATAFRAMES WHEN THE FOURTH MODEL IS ADDED! 
# Update pathway as needed; uncomment as needed
Enviro <- readRDS("Enviro_all_models_rcp45.RDS")
#Enviro <- readRDS("Enviro_all_models_rcp85.RDS")

## Need 10-year average of environmental data (1901-1911) for the initial HSI predictions
# This is the saved df that has already been averaged; the script to perform the calculations is in folder "brt_calibration"
Yr10_Ann <- readRDS("Yr10_Ann.RDS")

### 2.2. Catchment-specific data:
## Need the 'All_Basins' df that contains basin-specific data for all basins in EuroDiad v.4 
# Update pathway as needed
All_Basins <- readRDS("Info_All_Basins.RDS")

## Need the 'outletDistanceMatrix' matrix that stores pairwise distances between all catchments in the Atlantic Area
load("distanceMatrix15march.Rdata")

### 2.3. Species-specific data:
## Need the 'Survey' df that stores the output from the expert survey for all 11 species
# Variables from the survey or FishBase and are stored in excel file in folder "survey":
Survey<- read.xlsx("Species_Parameters.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE,
                   colClasses = (Response = "character"), stringsAsFactors = FALSE)
Survey <- Survey %>%
  mutate_at(c("y", "kpa", "Dmax", "r", "Fsurv", "AveLambda"), as.numeric)
## Set rownames to subset later
rownames(Survey) <- Survey$Lname

## For each species, need to source the R file storing the metaparameters for the calibrated brtModel (Uncomment as needed)
# Uncomment and update pathways as needed
Alosa1 <- readRDS("Alosa_tc1_NoNile.RDS")
#Fallax1 <- readRDS("Fallax_tc1_NoNile.RDS")
#Sturio1 <- readRDS("Sturio_tc1_NoNile.RDS")
#Anguilla1 <- readRDS("Anguilla_tc1_NoNile.RDS")
#Lampetra1 <- readRDS("Lampetra_tc1_NoNile.RDS")
#Liza1 <- readRDS("Liza_tc1_NoNile.RDS")
#Osmer1 <- readRDS("Osmer_tc1_NoNile.RDS")
#Petro1 <- readRDS("Petro_tc1_NoNile.RDS")
#Platich1 <- readRDS("Platich_tc1_NoNile.RDS")
#Salar1 <- readRDS("Salar_tc1_NoNile.RDS")
#Trutta1 <- readRDS("Trutta_tc1_NoNile.RDS")

#### Section 3: Prepare dataframes and parameters for use as input for model runs.---------------------------------

### 3.1. Catchment-specific lists:
## Subset distance matrix to remove rows and columns for "Altaelv" and "Tana" in Norway; we decided to exclude these catchments
outletDistanceMatrix <- outletDistanceMatrix[!rownames(outletDistanceMatrix) %in% c("Altaelv", "Tana"),
                                             !colnames(outletDistanceMatrix) %in% c("Altaelv", "Tana")]

## Create a list to store only the catchments in the Atlantic Area (as included in the distance matrix)
# This is used in the functions below to subset the catchment-specific information
# Convert distance matrix to dataframe, or won't work with tidy
DF.df <- data.frame(outletDistanceMatrix)
 
## Switch row names to column, then merge with basin dataframe to only include AA basins
# Has to be merged by basin name instead of basin id b/c outlet name is what is defined in distance matrix
AAbasins <- DF.df %>%
  dplyr::select(sort(names(.))) %>%
  rownames_to_column('Basin_name') %>%
  dplyr::select('Basin_name')

### 3.2. Species-specific parameter lists:
## Define the species and the corresponding brt calibration
brtModel = Alosa1

## Optional: Can clear the workspace, but keep outside dfs that are needed
#rm(list = setdiff(ls(), c('AAbasins', 'All_Basins', 'Enviro',
#                         'outletDistanceMatrix', 'Survey', 'brtModel', 'Yr10_Ann')))

## Define list of parameters to input for functions
# First need to define which species is being tested 
# in order to correctly subset the survey results
Species <- 'AAlosa'
Disp_parm <- list(
  
  ## Currently used in HSDM functions:
  
  ## Name of the current species
  Lname = Survey[Species, 'Lname'],
  ## 'Y' parameter from the survey (proportion of emigrants)
  # Use recalibrated value "y2"
  y = Survey[Species, 'y2'],
  ## 'A' parameter from the survey (alpha = scale parameter for dispersal kernel)
  # Use recalibrated value "alpha2"
  a = Survey[Species, 'alpha2'], 
  ## 'B' parameter from the survey (beta = shape parameter for dispersal kernel)
  # Use recalibrated value beta2
  b = Survey[Species, 'beta2'], 
  ## Lambda parameter from the survey (related to Allee effect)
  lambda = Survey[Species, 'AveLambda'],
  ## Max Density parameter from the survey (number of spawners per km2)
  Dmax = Survey[Species, 'Dmax'],
  ## 'R' parameter from the survey (growth rate, without taking into account anthropogenic mortality)
  r = Survey[Species, 'r'],
  ## 'eh1' parameter from the survey; currently set to eh1 = 1 (anthropogenic mortality)
  eh1 = exp(0),
  ## 'eh2' parameter from the survey; currently set to eh2 = 1 (anthropogenic mortality)
  eh2 = exp(0),
  ## FSurv parameter from the survey (survival applied only to emigrants)
  FSurv = Survey[Species, 'Fsurv'],
  ## Set the timeperiod to run the model for
  envYr = c(1951:2100),
  ## Specify the average generation time for the species (yrs).
  avAge = Survey[Species, 'avAge'], # Whole numbers and decimals accepted (ie: 2 or 2.5).
  ## Specify the number of years to split offspring among (max < avAge*2).
  bins = Survey[Species, 'cohorts'],
  ## Specify the rcp from enviro
  rcp = Enviro$Ann_Enviro_cn$rcp[1],
  ## MeanDist parameter from the survey
  MeanDist = Survey[Species, 'MeanDist'],
  
  ## Currently not used in functions (but could be varied for predict.gbm)
  ## Kappa parameter from the survey
  #kpa = Survey[Species, 'kpa'],
  ## 'tc' parameter from the survey
  #tc = Survey[Species, 'tc'],
  ## 'lr' parameter from the survey
  #lr = Survey[Species, 'lr'],
  ## 'bf' parameter from the survey
  #bf = Survey[Species, 'bf'],
  
  ### These need to be defined as 'yes' or 'no', or the functions will give a warning
  ## They need to be lowercase, or will give a warning
  ## Include an allee effect?
  allee = 'yes', #'no',
  ## Include "accidental" straying to natal catchment?
  NatalStray = 'yes', #'no',
  ## Use presence/absence data when initializing populations?
  UsePresence = 'no' #'yes'
  
)

#### Section 4: Read in the following functions: -------------------------------

#### Step 1: Subset for the Atlantic Area basins and set initial values of HSI: ---------

FUNbasininfo <- function(AAbasins, All_Basins, brtModel, Yr10_Ann){
  ### First, set up dataframe with 10-year average of environ data from 1901-1911 and basin-specific data
  ## This is to predict the initial HSI for all basins in the AA
  df10Ann <- merge(
    All_Basins[,c('basin_id', 'Surf', 'Length', 'Alt',
                  'Basin')],
    Yr10_Ann, 'basin_id', all.x = TRUE)
  
  ### Next, pull out information from calibrated brt (simplified model)
  pa <- data.frame(
    basin_id = brtModel$simplified_model$gbm.call$dataframe$basin_id,
    Basin_name = brtModel$simplified_model$gbm.call$dataframe$Basin.x,
    presence_absence = brtModel$simplified_model$gbm.call$dataframe$presence_absence
    )
  
  pajoin <- full_join(pa, df10Ann, by = "basin_id")
  
  ### Subset to only Atlantic Area basins
  tempdf <- pajoin %>%
    filter(Basin %in% AAbasins$Basin_name) %>%
    dplyr::select(-Basin_name) %>%
    rename("Basin_name" = Basin)
  
  ### Predict HSI for t0 for all Atlantic Area basins
  predictAA <- data.frame(
    tempdf,
    HSIt1 = predict.gbm(
      brtModel$simplified_model,
      tempdf,
      n.trees = brtModel$simplified_model$gbm.call$best.trees,
      type = "response")
  )
  
  ### Combine information
  basinbrt <- data.frame(
    Basin_name = predictAA$Basin_name,
    #basin_id = brtModel$simplified_model$gbm.call$dataframe$basin_id,
    HSIt1 = predictAA$HSIt1,
    presence_absence = predictAA$presence_absence,
    stringsAsFactors = FALSE
    )
  
  ### Merge the initial HSI with other basin information
  basininfo <- inner_join(basinbrt,
                          All_Basins,
                          by = c('Basin_name' = 'Basin'))
  
  ### Merge the basin info with AAbasins, keep all rows of AAbasins
  InitProb <- merge(AAbasins, basininfo, by = 'Basin_name',
                    sort = TRUE, all.x = TRUE)
  
  ### Filter out any rows with NA in the basin ID, basin name, initial HSI, or surface area 
  BasinInfo <- InitProb %>%
    filter_at(vars(c(basin_id, Basin_name, HSIt1, Surf)), all_vars(!is.na(.)))
  
  ### Select certain rows to make a smaller list (for testing purposes only).
  #BasinInfo <- BasinInfo[which(BasinInfo$country== 'Spain'),]
  
  ### Return the BasinInfo df.
  return(BasinInfo)
}


#### Step 2: Make sure that basins in distance matrix match basins in BasinInfo: --------------

### Create a function to subset the distance matrix to the basins needed for this species
 ## (very important if each species has a different number of basins being included).
FUNdistmatrix <- function(BasinInfo, outletDistanceMatrix){
  ### Filter distance matrix to only include rows and columns for basins
  ## that are in the species abundance table for this species:
  dmUse <- as.matrix(outletDistanceMatrix[c(BasinInfo$Basin_name),
                                          c(BasinInfo$Basin_name)])
  
  ### Replace the diagonal of the distance matrix with '0' so fish
  ## 'dispersing' to their home catchment have no added distance.
  diag(dmUse) <- 0
  
  ### Check that the row names in the distance matrix are ordered the same
  ## as in Basin_Info
  if(any(rownames(dmUse) != BasinInfo$Basin_name)) {
    print(paste("Warning! Basins are not ordered the same in the",
                "distance matrix and basin info dataframe!"))
    #} else {
    #print(paste("Internal check: Basin order is the same",
    #            "in distance matrix and Basin info dataframe"))
  }
  
  ### Check that the rownames and column names are in the same order as
  ## each other for the distance matrix. If not, print a warning.
  if(any(rownames(dmUse) != colnames(dmUse))){
    print("Warning! Row and column names are not equal in distance matrix")
    ### This code will switch the order of rows and columns in the distance
    ## matrix to match that of the Basin info df.
    dmUse <- dmUse[match(rownames(dmUse), BasinInfo$Basin_name),]
    dmUse <- dmUse[match(colnames(dmUse), BasinInfo$Basin_name),]
    #} else {
    #print(paste("Internal Check: rownames and column names",
    #            "are equal in distance matrix"))
  }
  
  ### Return the distance matrix.
  return(dmUse)
}

#### Step 3: Create all dispersal matrices: -----------------------------------

### Create a function to convert the estimated mean mortality during
## disperal between basins, to a mortality rate per km of dispersal
## by dividing the log of mean survival by the mean dispersal distance.
## (m is negative)
FUNm <- function(FSurv, MeanDist){
  if(FSurv > 0 & MeanDist > 0){
    m = log(FSurv) / MeanDist
  } else if(FSurv <= 0 & MeanDist > 0){
    m = log(0.001) / MeanDist
  } else if(FSurv > 0 & MeanDist <= 0){
    m = log(FSurv) / 0.001
  } else {
    m = log(0.001) / 0.001
  }
  return(m)
}


### Create a function to estimate the fraction of fish that survive
## disperal to another catchment based on the mortality rate per km.
## (m is negative)
FUNsurvivalMatrix <- function(m, dmUse){
  survivalMatrix = exp(m*dmUse)
  return(survivalMatrix)
}

### Create a function to estimate the proportion of emmigrants from
## each basin that will stray into each new catchment. The matrix is
## not expected to be symetrical.
FUNexpMatrix <- function(a, b, dmUse, NatalStray){

  ### The dispersal matrix can be calculated with or without allowing fish
  ## to 'accidentally stray' into their natal basin (allow values for when l = j).
  ## This is controlled by a yes/no statement (lowercase).
  if(NatalStray == 'yes'){
    ### Calculate the relative fraction of fish that would return to each
    ## basin using the parameters "a" and "b" from the survey.
    expMatrix2 <- exp(-a*(dmUse^b))
    
  }else if(NatalStray == 'no'){
    ### Calculate the relative fraction of fish that would return to each
    ## basin using the parameters "a" and "b" from the survey.
    expMatrix2 <- exp(-a*(dmUse^b))
    
    ### This places a zero in the diagonal (l = j), so no fish
    ## 'accidentally stray' into their natal catchment
    diag(expMatrix2) <- 0
    
  }else{
    ### Print a warning to alert the user.
    print(paste("Warning! Natal Straying parameter must be yes/no"))
    ### Create a NULL matrix of expMatrix2 to overwrite any existing value
    expMatrix2 <- NULL
  }
  
  ### Divide all rows in a column of expMatrix2 by the sum of the column.
  ## creates an absoulute dispersal fraction from the relative fractions.
  expMatrix <- expMatrix2 / colSums(expMatrix2)
  
  ## If you want to check values/diagnostics, try these:
  #expMatrix[1:5,1:5]
  #diag(expMatrix) # Should be 0s if natal stray = no
  #min(diag(expMatrix))
  #max(diag(expMatrix))
  #rowSums(expMatrix) # Should be 1s
  #colSums(expMatrix) # Should vary, but be in the ballpark of 1
  
  return(expMatrix)
}

#### Step 4: Create dataframes to hold data for a model run. --------------

### Create function to create empty fields for populations components
## for each climate model.
FUNdatafields <- function(BasinInfo, Disp_parm, dmUse){
  
  ### Specify how many additional columns are needed at the start of the
  ## model for initializing populations for a full generation.
  generationtime <- floor(Disp_parm$avAge - (Disp_parm$bins / 2) + Disp_parm$bins)
  
  ### Specify how many additional columns are needed for a 'burn-in' period.
  burnin <- 10
  
  ### Create an empty matrix that population components will be put in.
  emptymat <- matrix(NA, nrow = length(BasinInfo$Basin_name),
                     ncol = generationtime + burnin + length(Disp_parm$envYr))
  ## Name the rows for the basins.
  row.names(emptymat) = BasinInfo$Basin_name
  ## Name the columns for the years.
  colnames(emptymat) = c(
    paste(rep('Initial', generationtime), seq(1:generationtime), sep = ''),
    paste(rep('Burn', burnin), seq(1:burnin), sep = ''),
    Disp_parm$envYr)
  
  ### Create and name a list of matricies for soring data during a model run.
  fields <- list(
    ## Matrix for annual HSI values
    HSI  = emptymat,
    ## Matrix for Nit values, population size for each basin.
    Nit  = emptymat,
    ## Matrix for Njy values, number of emigrants from each basin.
    Njy  = emptymat,
    ## Matrix for DNjy values, the immigrants to each basin.
    DNjy = emptymat,
    ## Matrix for B1 values, the number of fish homing to each basin.
    B1   = emptymat,
    ## Matrix for Bit values, the total fish returning to each basin.
    Bit  = emptymat,
    ## Matrix for Min1 values, the maximum pop the envir. can support.
    Min1 = emptymat,
    ## Matrix for Min2 values, the maximum pop from population growth.
    Min2 = emptymat
  )
  
  ### Internal check to ensure that the population dataframe is the
  ## same order as the distance matrix.
  if(any(rownames(fields$HSI) != rownames(dmUse))){
    print("Warning! HSI basins do not match distance matrix")
    #} else {
    ## Row names match
    #print("Warning! HSI basins do not match distance matrix")
  }
  
  ## Return a set of empty field to fill.
  return(fields)
}

#### Step 5: Predict the HSI for the time series: ---------------------------

### Create a function to predict HSI for a given set of environmental data.
FUNpredHSI <- function(x, BasinInfo, brtModel, Disp_parm, enviro){
  
  ## For internal testing purposes:
  #enviro = Enviro$Ann_Enviro_cn
  #x = fields
  
  ### Filter the environmental data for each climate model by the basins
  ## and years that are needed for this species.
  tempEnv <-  enviro %>%
    group_by(basin_id) %>%
    dplyr::filter(basin_id %in% BasinInfo$basin_id) %>%
    dplyr::filter(year %in% Disp_parm$envYr)
  
  ### merge dataframe to get environ data and basin data together
  tempScen <- merge(
    BasinInfo[,c('basin_id', 'Surf', 'Length', 'Alt',
                 'Basin_name', 'presence_absence')],
    tempEnv, 'basin_id', all.x = TRUE)
  tempScen <- tempScen %>% arrange(year, Basin_name)
  
  ### Internal check to be sure that the basin names are correct
  if (all(tempScen$Basin_name != row.names(x$HSI))){
    print(paste("Warning! Different basin names in",
                "Environmental dataframe and distance matrix"))
  }
  
  ### Use the simplified model from brtModel in predict.gbm to get the
  ## fitted HSI values for each year.
  x$HSI[unique(tempScen$Basin_name),
        as.character(unique(tempScen$year))] <-
    matrix(
      predict.gbm(
        brtModel$simplified_model,
        tempScen,
        n.trees = brtModel$simplified_model$gbm.call$best.trees,
        type = "response"),
      nrow = length(unique(tempScen$Basin_name)),
      ncol = length(unique(tempScen$year)),
      byrow = FALSE
    )
  
  ### Set the HSI for the initial years from the "BasinInfo" dataframe.
  x$HSI[ ,grep('Initial', colnames(x$HSI))] <- BasinInfo$HSIt1
  ### Set the HSI for the burn-in years from the "BasinInfo" dataframe.
  x$HSI[ ,grep('Burn', colnames(x$HSI))] <- BasinInfo$HSIt1
  
  ### Internal check that the HSI matrix is filled.
  if(any(is.na(x$HSI))){
    print(paste('Warning! Some HSI values are NA'))
  }
  
  ## Return the dataset with the HSI matrix for the current climate model.
  return(x)
}

#### Step 6: Estimate the intial population size for each catchment. -----------

### This function creates initial populations for multiple year classes
## and calculates the initial population for each catchment, Nit.
FUNinitNit <- function(x, BasinInfo, Disp_parm){
  
  ### For internal check only:
  #x <- Clim_mod$Ann_Enviro_cn
  
  ### Check to see if populations should be limited by presence
  ## absence data.
  if(Disp_parm$UsePresence == 'yes'){
    ### Estimate an initial population size for the first year.
    x$Nit[,1] <-
      x$HSI[,1] * Disp_parm$Dmax * BasinInfo$Surf * Disp_parm$eh1 *
      BasinInfo$presence_absence
    
  } else if(Disp_parm$UsePresence == 'no'){
    ### Estimate an initial population size for the first year.
    x$Nit[,1] <-
      x$HSI[,1] * Disp_parm$Dmax * BasinInfo$Surf * Disp_parm$eh1
    
  } else {
    ## Parameter out of bounds. Return NAs to break the model.
    print('Warning! Allee effect parameter is not yes/no')
    ### Estimate an initial population size for the first year.
    x$Nit[,1] <- NA
  }
  
  ## Estimate inital populations for multiple year classes/bins.
  ## Check to see if the average age parameter is within bounds.
  if(is.numeric(Disp_parm$avAge) &
     Disp_parm$avAge >= 1){
    
    ## Check to see if the bins parameter is within bounds.
    if(is.numeric(Disp_parm$bins) &
       Disp_parm$bins >= 1 &
       Disp_parm$bins < 2 * Disp_parm$avAge){
      
      ## Fill in an initial population for all the columns of a
      ## complete generation.
      x$Nit[,colnames(x$Nit)[
        1:floor(Disp_parm$avAge - (Disp_parm$bins / 2) +
                  Disp_parm$bins)]] <- x$Nit[,1]
      
    } else if(is.numeric(Disp_parm$bins) &
              Disp_parm$bins >= 2 * Disp_parm$avAge){
      ## Cohort parameter out of bounds. Return NAs to break the model.
      print('Warning! Bins parameter is too large')
      ### Estimate an initial population size for the first year.
      x$Nit[,1] <- NA
    } else {
      ## Cohort parameter out of bounds. Return NAs to break the model.
      print('Warning! Bins parameter is out of bounds')
      ### Estimate an initial population size for the first year.
      x$Nit[,1] <- NA
    }
  } else {
    ## Timestep parameter out of bounds. Return NAs to break the model.
    print('Warning! Timestep parameter is not >= 1')
    ### Estimate an initial population size for the first year.
    x$Nit[,1] <- NA
  }
  
  ## Return the dataset with initial population values.
  return(x)
}


#### Step 7: Create lists for storing results for all models. ----------

### Create a function to make lists of matrices for storing the
## results of each model run.
FUNparm <- function(Clim_mod, Disp_parm, DMCombo){
  ### Create a dataframe that has a row for each combination
  ## of parameters to test.
  Parameters <- expand.grid(
    ## Call the list of all parameters except for: 'envYr'
    Disp_parm[names(Disp_parm) %in% c('envYr', 'avAge', 'bins') == FALSE],
    ## Drop the output attributes (stores the factor levels)
    KEEP.OUT.ATTRS = FALSE
  )
  
  ### All character fields were converted to factors. Convert them back.
  Parameters[sapply(Parameters, is.factor)] <-
    lapply(Parameters[sapply(Parameters, is.factor)], as.character)
  
  ### Create a list for the results of projecting the population through
  ## the years. This will currently give a nested list, with all climate
  ## models run for each set of parameters.
  Pop_results <- lapply(seq_along(row.names(Parameters)), FUN = function(x){
    ### Create mostly empty maricies for populations.
    tempres <- Clim_mod
    ### Select a set of parameters to test
    tempres$ParmSet <- as.list(Parameters[x,])
    ### Include any parameters that were excluded from expand.grid
    tempres$ParmSet$envYr <- Disp_parm$envYr
    tempres$ParmSet$avAge <- Disp_parm$avAge
    tempres$ParmSet$bins <- Disp_parm$bins
    ### Include the Dispersal Matrix for the current paramenters.
    ## using: "a", "b", "NatalStray", "FSurv", "MeanDist"
    tempres$ParmSet$DispMatrix <-
      DMCombo[[as.character(tempres$ParmSet$a)]][[
        as.character(tempres$ParmSet$b)]][[
          as.character(tempres$ParmSet$NatalStray)]][[
            as.character(tempres$ParmSet$FSurv)]][[
              as.character(tempres$ParmSet$MeanDist)]]
    ### Return the result setup
    return(tempres)
  })
  ### Return the result setup
  return(Pop_results)
}

#### Step 8: Create a function to calculate population by time step. --------

### Create a function to calculate the population components and
## matricies for a given time step.
FUNpopCalc <- function(c, i, BasinInfo, parm){
  
  ## These are for internal testing purposes.
  #c = Clim_mod$Ann_Enviro_cn
  #parm <- Pop_results[[1]]$ParmSet
  #i = floor(parm$avAge - (parm$bins / 2) + parm$bins + 1)
  
  ## Create a variable for the name of the column for the current year.
  yr = colnames(c$HSI)[i]
  
  ## Create a variable for the names of the columns (bins) contributing
  ## to this generation.
  if(i > floor(parm$avAge - (parm$bins / 2) + parm$bins)){
    prevgen = colnames(c$HSI)[
      i - rev(floor(parm$avAge - (parm$bins / 2) + 1:parm$bins))
      ]
  } else {
    ## i is out of bounds. Return NAs to break the model.
    print('Warning! value of i in the population loop is too small.')
    ### Estimate an initial population size for the first year.
    #prevgen = NA # Suppress for now. If "i" is too small, model breaks anyway.
  }
  ### Calculate the population from the "A" side of the
  ### equation in each basin.
  ## Calculate the max pop, according to HSI "A".
  c$Min1[,yr] <- c$HSI[,yr] * parm$Dmax * BasinInfo$Surf * parm$eh1
  
  ### Calculate the population from the "B" side of the
  ### equation for each basin.
  ## Calculate the number of emigrants from each basin.
  c$Njy[,yr] <- rowSums(c$Nit[,prevgen] / parm$bins) * parm$y
  ## Calculate the number of immigrants to each basin.
  c$DNjy[,yr] <- t(c$Njy[,yr] %*% parm$DispMatrix)
  ## Calculate the number of fish returning to their natal basin.
  c$B1[,yr] <- rowSums(c$Nit[,prevgen] / parm$bins) * (1 - parm$y)
  ## Sum up the total spawners in each basin.
  c$Bit[,yr] <- (c$DNjy[,yr] + c$B1[,yr]) * parm$eh2
  
  ### Check if an allee effect should be used.
  if(parm$allee == 'yes'){
    ## Calculate the max pop, according to population growth "B",
    ## with an allee effect
    c$Min2[,yr] <- c$Bit[,yr] * parm$r *
      (c$Bit[,yr]^2 /
         (c$Bit[,yr]^2 +
            (parm$lambda * parm$Dmax * BasinInfo$Surf)^2))
    
  } else if(parm$allee == 'no'){
    ## Calculate the max pop, according to population growth "B",
    ## without an allee effect
    c$Min2[,yr] <- c$Bit[,yr] * parm$r
    
  } else {
    ## Parameter out of bounds. Return NAs to break the model.
    print('Warning! Allee effect parameter is not yes/no')
    c$Min2[,yr] <- NA
  }
  
  ## Compare the results for each basin, and keep the MINIMUM
  ## value as Nit for this year.
  c$Nit[,yr] <- pmin(c$Min1[,yr], c$Min2[,yr])
  ## Tounds the results for Nit down to 0 fish if there are 2 or less fish present;
  # (so that we don't have less than 1 fish that is being counted as presence)
  c$Nit[c$Nit[,yr] <= 2, yr] <- 0
  
  ## Return the results for the year.
  return(c)
}

#### Step 9: Combine all of the smaller fumctions to calculate the populations and dispersal-------------

### Use all of the smaller function to calculate populations.
dispersalFunc <- function(AAbasins, All_Basins, brtModel, Disp_parm,
                          Enviro, outletDistanceMatrix, Yr10_Ann){
  #### Step 1: Create a list of the basins needed for this species: ------------
  
  ### Create the Basin Info df from FUNbasininfo
  BasinInfo <- FUNbasininfo(AAbasins = AAbasins,
                            All_Basins = All_Basins,
                            brtModel = brtModel,
                            Yr10_Ann = Yr10_Ann)
  
  #### Step 2: Filter the distance matrix for this species: --------------------
  
  ### Create the distance matrix from FUNdistmatrix
  dmUse <- FUNdistmatrix(BasinInfo = BasinInfo,
                         outletDistanceMatrix = outletDistanceMatrix)
  
  #### Step 3: Create all dispersal matricies: ---------------------------------
  
  ### Make a nested list to calculate expected dispersal matricies for
  ## the combinations of alpha, beta, and natal straying provided.
  ## For each alpha:
  DMCombo <- lapply(seq_along(Disp_parm$a), FUN = function(A){
    ## For each beta:
    tempb <- lapply(seq_along(Disp_parm$b), FUN = function(B){
      ## For each natal straying:
      tempstray <- lapply(seq_along(Disp_parm$NatalStray), FUN = function(C){
        ## Calculate and return a dispersal matrix.
        tempexp <- FUNexpMatrix(a = Disp_parm$a[A],
                                b = Disp_parm$b[B],
                                dmUse = dmUse,
                                NatalStray = Disp_parm$NatalStray[C])
        ### Make a nested list to calculate survival matricies for the
        ## combinations of survival and distances provided.
        ## For each mean survival:
        tempcom <- lapply(seq_along(Disp_parm$FSurv), FUN = function(f){
          ## For each mean distance:
          tempf <- lapply(seq_along(Disp_parm$MeanDist), FUN = function(d){
            ## Calculate mortality
            tempm <- FUNm(FSurv = Disp_parm$FSurv[f],
                          MeanDist = Disp_parm$MeanDist[d])
            ## Calculate a survival matrix
            tempsur <- FUNsurvivalMatrix(m = tempm, dmUse = dmUse)
            ## Multiply element-wise the sxpMatrix and survival matricies.
            dm <- tempexp * tempsur
            ## Return the dispersal matrix
            return(dm)
          })
          ## Name the list of survival matricies by mean distance
          names(tempf) <- Disp_parm$MeanDist
          ## Return the named list.
          return(tempf)
        })
        ## Name the list of matricies by mean survival
        names(tempcom) <- Disp_parm$FSurv
        ## Return the named list.
        return(tempcom)
      })
      ## Name the list by natal straying.
      names(tempstray) <- Disp_parm$NatalStray
      ## Return the named list.
      return(tempstray)
    })
    ## Name the list by beta.
    names(tempb) <- Disp_parm$b
    ## Return the named list.
    return(tempb)
  })
  ## Name the list by alpha.
  names(DMCombo) <- Disp_parm$a
  
  ### INTERNAL CHECK: When m is set to 0, DMCombo should be the exact
  ## same as expMatrix b/c all values in survivalMatrix are 1.
  ## Also, when m is set to zero, the sum of the values for each
  ## catchment in expVar should equal 1. This is because the total number
  ## of emigrants being produced by catchment j should equal the number of
  ## emigrants that enter each of the other catchments. This matrix is not
  ## symetrical because the number immigrants to a given catchment is
  ## independant of the number of emmigrants from that catchment.
  ## I.e: rowsums != colsums.
  
  #### Step 4: Create dataframes to hold data for a model run. -----------------
  
  ### Create a set of empty fields for storing population components.
  fields <- FUNdatafields(BasinInfo = BasinInfo,
                          Disp_parm = Disp_parm,
                          dmUse = dmUse)
  
  
  #### Step 5: Predict the HSI for the time series: ----------------------------
  
  ### Use the HSI function to predict HSI for each climate model.
  Clim_mod <- lapply(seq_along(Enviro), FUN = function(x){
    tempHSI <- FUNpredHSI(x = fields,
                          BasinInfo = BasinInfo,
                          brtModel = brtModel,
                          Disp_parm = Disp_parm,
                          enviro = Enviro[[x]])
    
    ## Return the dataset with the HSI matricies.
    return(tempHSI)
  })
  ## Add names to the list.
  names(Clim_mod) <- names(Enviro)
  
  
  #### Step 6: Estimate the intial population size for each catchment. ---------
  
  ### Calculate the initial population for all the climate models.
  Clim_mod <- lapply(Clim_mod, FUN = function(x){
    FUNinitNit(x, BasinInfo = BasinInfo, Disp_parm = Disp_parm)
  })
  
  #### Step 7: Create lists for storing results for all models. ----------------
  
  ### Create empty results tables to store the models.
  Pop_results <- FUNparm(Clim_mod = Clim_mod,
                         Disp_parm = Disp_parm,
                         DMCombo = DMCombo)
  
  #### Step 8+9: Run the FUNpopCalc function to calc poplulations.--------------
  
  ### Initialize a progress bar for keeping track of progress
  progbar <- txtProgressBar(min = 1,
                            max = ncol(fields$HSI),
                            style = 3)
  
  ### Run FUNpopcalc for the selected years.
  for(i in
      floor(Disp_parm$avAge - (Disp_parm$bins / 2) + Disp_parm$bins + 1):
      ncol(fields$HSI)){
    
    ## To run the population function for all combinations for a single year:
    #i = 1950
    ## A short script to print a progress bar to the console for your benefit.
    setTxtProgressBar(progbar, i)
    
    ## Name the object that the results of the loop will be stored in.
    Pop_results <- lapply(Pop_results, FUN = function(x){
      
      ## To try it manually:
      #x <- Pop_results[[1]]
      
      ### Due the follow for all elements of the list except parameters
      x[names(x)[names(x) %in% c('ParmSet') == FALSE]] <-
        lapply(x[names(x)[names(x) %in% c('ParmSet') == FALSE]],
               FUN = function(c){
                 FUNpopCalc(c = c, i = i,
                            BasinInfo = BasinInfo,
                            parm = x$ParmSet)
               })
      
      ### Return the resulting populations for a set of parameters.
      return(x)
    })
  }
  
  ### This version will run all of the climate models for a
  ## single set of parameters.
  
  ### Create a list for the results of the population projection.
  #Clim_results <- Clim_mod
  
  ### Run the new function for the selected years.
  #for(i in Disp_parm$envYr){
  #  Clim_results <- lapply(Clim_results, FUN = function(c){
  #    FUNpopCalc(c = c, i = i,
  #               parm = Disp_parm,
  #               BasinInfo = BasinInfo)
  #  })
  #}
  
  ### Here are a few lines of code to extract individual model runs and check
  ## if the results are the same as other models.
  ### Take a look at the corners of a matrix
  #Pop_results[[43]]$Ann_Enviro_cs$Nit[c(1:5,97:102), c(1:5,149:151)]
  ### Check for NA: if True, then data is missing
  #any(is.na(Pop_results[[1]]))
  ### Test if HSI is the same in adjacent models (only changed climate)
  ## Should be false
  #Pop_results[[1]]$Ann_Enviro_cn$HSI == Pop_results[[1]]$Ann_Enviro_cs$HSI
  ### Test if pop is the same in adjacent models (only changed climate)
  ## Should be false
  #Pop_results[[1]]$Ann_Enviro_cn$Nit == Pop_results[[1]]$Ann_Enviro_cs$Nit
  ### Test if HSI is the same in nonadjacent models (same climate)
  ## Should be true
  #Pop_results[[1]]$Ann_Enviro_cn$HSI == Pop_results[[4]]$Ann_Enviro_cn$HSI
  ### Test if pop is the same in nonadjacent models (same climate)
  ## Should be false
  #Pop_results[[1]]$Ann_Enviro_cn$Nit == Pop_results[[4]]$Ann_Enviro_cn$Nit
  
  ### Make a list of results to return from the dispersal function.
  return(list(Pop_results, BasinInfo))
}


#### Section 5: Run the whole function and view results. -------------------------

### Code for running the whole function
results <- dispersalFunc(AAbasins = AAbasins,
                         All_Basins = All_Basins,
                         brtModel = brtModel,
                         Disp_parm = Disp_parm,
                         Enviro = Enviro,
                         outletDistanceMatrix = outletDistanceMatrix,
                         Yr10_Ann = Yr10_Ann)

## NOTE 1: When looking at the results, the first 6 columns (labelled "Initial1" - "Initial6")
# SHOULD be NA for Njy, DNjy, B1, Bit, Min1, and Min2. It doesn't not mean the model isn't working. 
# Calculated values should start appearing in all of these dataframes 
# starting in column "Burn1"

## NOTE 2: Script to subset the results and create heatplots is in file "Subset_heatplot.R"




