## --------------------------- #
##
## Script name: HyDiaD,  Hybrid Species Distribution Model for Diadromous fish
##
## Purpose of script: This function uses the equations developed by Betsy Barber and Patrick
## Lambert to estimate spawner abundance for each catchment i at time t
##
## Author: Dr. Betsy Barber, 
##  Modified by Patrick Lambert
## Date Created: 2020-09-21
## Date Updated: 2021-05-20
##
## Copyright (c) Betsy Barber, 
##               Patrick Lambert
## Email: patrick.mh.lambert@inrae.fr
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

## ---------------------------#
## Notes: Must specify the following dataframes: 
##
## 1) Projected environmental predictor variables from 1951-2100
## 2) Historical observed environmental predictor variables as mean values from 1901-1911
## 3) Catchment-specific information from EuroDiad v.4
## 4) Distance matrix storing all pairwise distances between catchments in the Atlantic Area
## 5) Species-specific parameters (as weighted means) from expert survey
## 6) Species-specific metaparameters from calibrated brtModel (simplified model)

## ---------------------------#

# Section 1: Load the packages we will need:  ----
library(gbm)

library(readxl)
library(dismo)
library(tidyverse)

## Clear the workspace:
rm(list = ls())

# Section 2: Load the data files that will be needed. ---------

# set the working directory:
# setwd("C:/Users/betsy.barber/Work Folders/Documents/HDSM/HSDM_Script_Full/HDSM_all_files/HSDM")

## 2.1. Species-specific data ----
## Need the 'Survey' df that stores the output from the expert survey for all 11 species

suffix = 'HyDiaD'
HyDiaDParameter <- read_rds("./data_input/HyDiaDParameter.rds")

# suffix = 'Betsy'
# HyDiaDParameter <- read_rds("./data_input/BetsyParameter.rds")
#
# suffix = 'default'
# HyDiaDParameter <- read_rds("./data_input/HyDiaDParameter_default.rds")


## 2.2. Environmental predictor variables: ----
# Climate data from three climate models - df is projected monthly values from 1951-2100
#   WILL NEED TO UPDATE THESE DATAFRAMES WHEN THE FOURTH MODEL IS ADDED! 
#   Update pathway as needed; 
rcp = HyDiaDParameter %>% distinct(rcp)
Enviro <- read_rds(paste0("data_input/Enviro_all_models_",rcp,".RDS"))

# Need 10-year average of environmental data (1901-1911) for the initial HSI predictions
#  This is the saved df that has already been averaged; the script to perform the calculations is in folder "brt_calibration"
Yr10_Ann <- read_rds("data_input/Yr10_Ann.RDS")

##  2.3. Catchment-specific data: ----
## Need the 'All_Basins' df that contains basin-specific data for all basins in EuroDiad v.4 
# Update pathway as needed
All_Basins <- read_rds("data_input/Info_All_Basins.RDS")

## Need the 'outletDistanceMatrix' matrix that stores pairwise distances between all catchments in the Atlantic Area
load("data_input/distanceMatrix15march.Rdata")

## 2.4. Source BRT with species  ----
## For each species, need to source the R file storing the metaparameters for the calibrated brtModel
Species_BRT <- readxl::read_xlsx("data_input/Species_BRT.xlsx", sheet = 1) 


# Section 3: Prepare dataframes and parameters for use as input for model runs.---------------------------------
## Subset distance matrix to remove rows and columns for "Altaelv" and "Tana" in Norway; we decided to exclude these catchments
#TODO update the distanceMatrix, use as a longer tibble ?
outletDistanceMatrix <- outletDistanceMatrix[!rownames(outletDistanceMatrix) %in% c("Altaelv", "Tana"),
                                             !colnames(outletDistanceMatrix) %in% c("Altaelv", "Tana")]

## Create a list to store only the catchments in the Atlantic Area (as included in the distance matrix)
# This is used in the functions below to subset the catchment-specific information
# Convert distance matrix to dataframe,

# DF.df <- data.frame(outletDistanceMatrix)
DF.df <- outletDistanceMatrix %>%  
  as_tibble(rownames = 'Basin_name') %>% 
  arrange(Basin_name)

# column_to_rownames('departure')


## Switch row names to column, then merge with basin dataframe to only include AA basins
# Has to be merged by basin name instead of basin id b/c outlet name is what is defined in distance matrix
# AAbasins <- DF.df %>%
#   dplyr::select(sort(names(.))) %>%
#   rownames_to_column('Basin_name') %>%
#   dplyr::select('Basin_name')

AAbasins <- DF.df %>% 
  arrange(Basin_name) %>% 
  dplyr::select(Basin_name)

# AAbasins %>% anti_join(All_Basins, by = c("Basin_name" = "Basin"))

# Section 4: Build subfunctions to be used in simulations: -------------------------------

## Step 1: Subset for the Atlantic Area basins and set initial values of HSI: ---------

FUNbasininfo <- function(AAbasins, All_Basins, brtModel, Yr10_Ann){
  
  BasinInfo <-  AAbasins %>% 
    # add basin feature
    inner_join(All_Basins,
               by = c('Basin_name' = 'Basin')) %>% 
    # add 10-year average of environ data from 1901-1911 
    inner_join(Yr10_Ann, by = 'basin_id') %>% 
    # add presence_absence information from calibrated brt (simplified model)
    left_join(brtModel$simplified_model$gbm.call$dataframe %>% 
                dplyr::select(basin_id, presence_absence),
              by = 'basin_id') %>%
    # replace presence_absence = NA by 0
    replace_na(list(presence_absence = 0)) %>% 
    # predict HSI for t0
    mutate(HSIt1 = predict.gbm(
      object = brtModel$simplified_model,
      newdata = .,
      n.trees = brtModel$simplified_model$gbm.call$best.trees,
      type = "response")) %>%
    # sort by Basin_name
    arrange(Basin_name) %>% 
    # drop basin with missing HSIt1 and surf
    drop_na(any_of(c("HSIt1", "Surf")))
    
  
  return(BasinInfo)
}

# test: BasinInfo <- FUNbasininfo(AAbasins, All_Basins, brtModel, Yr10_Ann)

## Step 2: Make sure that basins in distance matrix match basins in BasinInfo: --------------

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
  if (any(rownames(dmUse) != colnames(dmUse))) {
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
# to test
# dmUse <- FUNdistmatrix(BasinInfo, outletDistanceMatrix)
# dmUse['Aa', 1:10]

## Step 3: Create all dispersal matrices: -----------------------------------

### Create a function to estimate the fraction of fish that survive
## disperal to another catchment based on the mortality rate per km.
FUNsurvivalMatrix <- function(dmUse, Sdisp, DistMean){
  ###  calculate mortality rate per km of dispersal
  ## by dividing the minus of log of mean survival by the mean dispersal distance.
  
  # if(Sdisp > 0 & DistMean > 0){
  #   Msurv = - log(Sdisp) / DistMean
  # } else if(Sdisp <= 0 & DistMean > 0){
  #   Msurv =  -log(0.001) / DistMean
  # } else if(Sdisp > 0 & DistMean <= 0){
  #   Msurv =  -log(Sdisp) / 0.001
  # } else {
  #   Msurv = - log(0.001) / 0.001
  # }
  
  if (Sdisp > 0 & DistMean > 0) {
    Msurv = -log(Sdisp) / DistMean
  } else {
    Msurv = 0
  }
  
  # survival matrix according to mortality coefficient and distances
  survivalMatrix = exp(-Msurv * dmUse)
  return(survivalMatrix)
}

### Create a function to estimate the proportion of emigrants from
## each basin that will stray into each new catchment. The matrix is
## not expected to be symetrical.
FUNemigrantMatrix <- function(dmUse, alpha, beta,  withNatalStray){
  ### Calculate the relative fraction of fish that would return to each
  ## basin 
  ### The dispersal matrix can be calculated with or without allowing fish
  ## to 'accidentally stray' into their natal basin (allow values for when l = j).
  
  # calculate the weight of each catchment according to the kernal function
  expMatrix2 <- exp(-alpha * (dmUse ^ beta))
  
  if (withNatalStray == FALSE) {
    ### This places a zero in the diagonal (l = j), so no fish
    ## 'accidentally stray' into their natal catchment
    diag(expMatrix2) <- 0
  }
  
  ### Divide all rows in a column of expMatrix2 by the sum of the column.
  ## creates a dispersal fraction from the catchment weight.
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

# to test
parameter <- HyDiaDParameter %>% filter(latin_name == 'Alosa alosa')
FUNsurvivalMatrix(dmUse, Sdisp= parameter$Sdisp, DistMean = parameter$DistMean)['Aa', 1:10]
FUNemigrantMatrix(dmUse, alpha =  parameter$alpha, 
                  beta = parameter$beta, 
                  withNatalStray = parameter$withNatalStray)['Aa', 1:10]
  
## Step 4: Create empty dataframes to hold data for a model run. --------------

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
  #TODO simplify
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
  if (any(rownames(fields$HSI) != rownames(dmUse))) {
    print("Warning! HSI basins do not match distance matrix")
    #} else {
    ## Row names match
    #print("Warning! HSI basins do not match distance matrix")
  }
  
  ## Return a set of empty field to fill.
  return(fields)
}

## Step 5: Predict the HSI for the time series: ---------------------------

### Create a function to predict HSI for a given set of environmental data.
FUNpredHSI <- function(output, BasinInfo, brtModel, Disp_parm, enviro){
  
  ## For internal testing purposes:
  #enviro = Enviro$Ann_Enviro_cn
  #output = fields
  
  ### Filter the environmental data for each climate model by the basins
  ## and years that are needed for this species.
  tempEnv <-  enviro %>%
    group_by(basin_id) %>%
    dplyr::filter(basin_id %in% BasinInfo$basin_id) %>%
    dplyr::filter(year %in% Disp_parm$envYr)
  
  ### merge dataframe to get environ data and basin data together
  tempScen <- BasinInfo %>% 
    dplyr::select(basin_id, Surf, Length, Alt, Basin_name, presence_absence) %>% 
    left_join(tempEnv, by = 'basin_id') %>% 
    arrange(year, Basin_name)
  
  ### Internal check to be sure that the basin names are correct
  if (all(tempScen$Basin_name != row.names(output$HSI))) {
    print(paste("Warning! Different basin names in",
                "Environmental dataframe and distance matrix"))
  }
  
  ### Use the simplified model from brtModel in predict.gbm to get the
  ## fitted HSI values for each year.
  output$HSI[unique(tempScen$Basin_name),
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
  output$HSI[ ,grep('Initial', colnames(output$HSI))] <- BasinInfo$HSIt1
  ### Set the HSI for the burn-in years from the "BasinInfo" dataframe.
  output$HSI[ ,grep('Burn', colnames(output$HSI))] <- BasinInfo$HSIt1
  
  ### Internal check that the HSI matrix is filled.
  if(any(is.na(output$HSI))){
    print(paste('Warning! Some HSI values are NA'))
  }
  
  ## Return the dataset with the HSI matrix for the current climate model.
  return(output)
}

## Step 6: Estimate the intial population size for each catchment. -----------

### This function creates initial populations for multiple year classes
## and calculates the initial population for each catchment, Nit.
FUNinitNit <- function(output, BasinInfo, Disp_parm){
  
  ### For internal check only:
  #output <- Clim_mod$Ann_Enviro_cn
  
  output$Nit[,1] <- NA
  ### Check to see if populations should be limited by presence
  ## absence data.
  if (Disp_parm$usePresence == TRUE) {
    ### Estimate an initial population size for the first year.
    output$Nit[,1] <-
      output$HSI[,1] * Disp_parm$Dmax * BasinInfo$Surf * Disp_parm$eh1 *
      BasinInfo$presence_absence
    
  } else if(Disp_parm$usePresence == FALSE){
    ### Estimate an initial population size for the first year.
    output$Nit[,1] <-
      output$HSI[,1] * Disp_parm$Dmax * BasinInfo$Surf * Disp_parm$eh1
  } 
  
  ## Estimate initial populations for multiple year classes/bins.
  ## Check to see if the average age parameter is within bounds.
  if (Disp_parm$avAge >= 1) {
    
    ## Check to see if the bins parameter is within bounds.
    if (Disp_parm$bins >= 1 &
        Disp_parm$bins < 2 * Disp_parm$avAge) {
      
      ## Fill in an initial population for all the columns of a
      ## complete generation.
      output$Nit[, colnames(output$Nit)[
        1:floor(Disp_parm$avAge - (Disp_parm$bins / 2) +
                  Disp_parm$bins)]] <- output$Nit[,1]
      
    } else if (Disp_parm$bins >= 2 * Disp_parm$avAge) {
      ## Cohort parameter out of bounds. Return NAs to break the model.
      print('Warning! Bins parameter (number of cohorts) is too large according to age at first maturity ')
      ### Estimate an initial population size for the first year.
      output$Nit[,1] <- NA
    } else {
      ## Cohort parameter out of bounds. Return NAs to break the model.
      print('Warning! Bins parameter is out of bounds')
      ### Estimate an initial population size for the first year.
      output$Nit[,1] <- NA
    }
  } else {
    ## Timestep parameter out of bounds. Return NAs to break the model.
    print('Warning! avAge (age at first maturity) parameter is not >= 1')
    ### Estimate an initial population size for the first year.
    output$Nit[,1] <- NA
  }
  
  ## Return the dataset with initial population values.
  return(output)
}


## Step 7: Create lists for storing results for all models. ----------

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
  ##PL WHY
  Parameters[sapply(Parameters, is.factor)] <-
    lapply(Parameters[sapply(Parameters, is.factor)], as.character)
  
  ### Create a list for the results of projecting the population through
  ## the years. This will currently give a nested list, with all climate
  ## models run for each set of parameters.
  Pop_results <- lapply(seq_along(row.names(Parameters)), function(x){
    ### Create mostly empty matrices for populations.
    tempres <- Clim_mod
    ### Select a set of parameters to test
    tempres$ParmSet <- as.list(Parameters[x,])
    ### Include any parameters that were excluded from expand.grid
    tempres$ParmSet$envYr <- Disp_parm$envYr
    tempres$ParmSet$avAge <- Disp_parm$avAge
    tempres$ParmSet$bins <- Disp_parm$bins
    ### Include the Dispersal Matrix for the current paramenters.
    ## using: "alpha", "beta", "withNatalStray", "Sdisp", "DistMean"
    tempres$ParmSet$DispMatrix <-
      DMCombo[[as.character(tempres$ParmSet$alpha)]][[
        as.character(tempres$ParmSet$beta)]][[
          as.character(tempres$ParmSet$withNatalStray)]][[
            as.character(tempres$ParmSet$Sdisp)]][[
              as.character(tempres$ParmSet$DistMean)]]
    ### Return the result setup
    return(tempres)
  })
  ### Return the result setup
  return(Pop_results)
}

## Step 8: Create a function to calculate population by time step. --------

### Create a function to calculate the population components and
## matrices for a given time step.
FUNpopCalc <- function(c, i, BasinInfo, parm){
  
  ## These are for internal testing purposes.
  # c = Clim_mod$Ann_Enviro_cn
  # parm <- Pop_results[[1]]$ParmSet
  # i = floor(parm$avAge - (parm$bins / 2) + parm$bins + 1)
  
  ## Create a variable for the name of the column for the current year.
  yr = colnames(c$HSI)[i]
  
  ## Create a variable for the names of the columns (bins) contributing
  ## to this generation.
  if (i > floor(parm$avAge - (parm$bins / 2) + parm$bins)) {
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
  c$Njy[,yr] <- rowSums(c$Nit[,prevgen] / parm$bins) * parm$gamma
  ## Calculate the number of immigrants to each basin.
  c$DNjy[,yr] <- t(c$Njy[,yr] %*% parm$DispMatrix)
  ## Calculate the number of fish returning to their natal basin.
  c$B1[,yr] <- rowSums(c$Nit[,prevgen] / parm$bins) * (1 - parm$gamma)
  ## Sum up the total spawners in each basin.
  c$Bit[,yr] <- (c$DNjy[,yr] + c$B1[,yr]) * parm$eh2
  
  ### Check if an Allee effect should be used.
  if(parm$withAllee == TRUE){
    ## Calculate the max pop, according to population growth "B",
    ## with an Allee effect
    c$Min2[,yr] <- c$Bit[,yr] * parm$r *
      (c$Bit[,yr]^2 /
         (c$Bit[,yr]^2 +
            (parm$lambda * parm$Dmax * BasinInfo$Surf)^2))
    
  } else if(parm$withAllee == FALSE){
    ## Calculate the max pop, according to population growth "B",
    ## without an Allee effect
    c$Min2[,yr] <- c$Bit[,yr] * parm$r
    
  } else {
    ## Parameter out of bounds. Return NAs to break the model.
    print('Warning! Allee effect parameter is not TRUE/FALSE')
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

## Step 9: Combine all functions to calculate the populations and dispersal-------------

dispersalFunc <- function(AAbasins, All_Basins, brtModel, Disp_parm,
                          Enviro, outletDistanceMatrix, Yr10_Ann){
  
  ### Step 9.1: Create a list of the basins needed for this species ----
  BasinInfo <- FUNbasininfo(AAbasins = AAbasins,
                            All_Basins = All_Basins,
                            brtModel = brtModel,
                            Yr10_Ann = Yr10_Ann)
  
  ### Step 9.2: Create the distance matrix ----------------------------------------
  dmUse <- FUNdistmatrix(BasinInfo = BasinInfo,
                         outletDistanceMatrix = outletDistanceMatrix)
  
  ### Step 9.3: Create all dispersal matricies: -----------------------------------
  
  ### Make a nested list to calculate expected dispersal matricies for
  ## the combinations of alpha, beta, and natal straying provided.
  ## For each alpha:
  DMCombo <- lapply(seq_along(Disp_parm$alpha), function(i_alpha){
    ## For each beta:
    tempb <- lapply(seq_along(Disp_parm$beta), function(i_beta){
      ## For each natal straying:
      tempstray <- lapply(seq_along(Disp_parm$withNatalStray), function(i_natalStray){
        ## Calculate and return a dispersal matrix.
        tempexp <- FUNemigrantMatrix(dmUse = dmUse,
                                     alpha = Disp_parm$alpha[i_alpha],
                                     beta = Disp_parm$beta[i_beta],
                                     withNatalStray = Disp_parm$withNatalStray[i_natalStray])
        ### Make a nested list to calculate survival matrices for the
        ## combinations of survival and distances provided.
        ## For each mean survival:
        tempcom <- lapply(seq_along(Disp_parm$Sdisp), function(i_Sdisp){
          ## For each mean distance:
          tempf <- lapply(seq_along(Disp_parm$DistMean), function(i_DistMean){
            ## Calculate a survival matrix
            tempsur <- FUNsurvivalMatrix(dmUse = dmUse, 
                                         Sdisp = Disp_parm$Sdisp[i_Sdisp],
                                         DistMean = Disp_parm$DistMean[i_DistMean])
            ## Multiply element-wise the sxpMatrix and survival matricies.
            dm <- tempexp * tempsur
            ## Return the dispersal matrix
            return(dm)
          })
          ## Name the list of survival matricies by mean distance
          names(tempf) <- Disp_parm$DistMean
          ## Return the named list.
          return(tempf)
        })
        ## Name the list of matricies by mean survival
        names(tempcom) <- Disp_parm$Sdisp
        ## Return the named list.
        return(tempcom)
      })
      ## Name the list by natal straying.
      names(tempstray) <- Disp_parm$withNatalStray
      ## Return the named list.
      return(tempstray)
    })
    ## Name the list by beta.
    names(tempb) <- Disp_parm$beta
    ## Return the named list.
    return(tempb)
  })
  ## Name the list by alpha.
  names(DMCombo) <- Disp_parm$alpha
  
  ### INTERNAL CHECK: When m is set to 0, DMCombo should be the exact
  ## same as expMatrix b/c all values in survivalMatrix are 1.
  ## Also, when m is set to zero, the sum of the values for each
  ## catchment in expVar should equal 1. This is because the total number
  ## of emigrants being produced by catchment j should equal the number of
  ## emigrants that enter each of the other catchments. This matrix is not
  ## symetrical because the number immigrants to a given catchment is
  ## independant of the number of emmigrants from that catchment.
  ## I.e: rowsums != colsums.
  
  ### Step 9.4: Create empty dataframes to hold data for a model run: -----------------
  fields <- FUNdatafields(BasinInfo = BasinInfo,
                          Disp_parm = Disp_parm,
                          dmUse = dmUse)
  
  
  ### Step 9.5: Predict the HSI for the time series for each climate model: ----------------------------
  Clim_mod <- lapply(seq_along(Enviro), function(i_model){
    tempHSI <- FUNpredHSI(output = fields,
                          BasinInfo = BasinInfo,
                          brtModel = brtModel,
                          Disp_parm = Disp_parm,
                          enviro = Enviro[[i_model]])
    return(tempHSI)
  })
  ## Add names to the list.
  names(Clim_mod) <- names(Enviro)
  
  ### Step 9.6: Estimate the initial population size for each catchment and for  all the climate models. ---------
  Clim_mod <- lapply(Clim_mod, function(x){
    FUNinitNit(output = x, 
               BasinInfo = BasinInfo, 
               Disp_parm = Disp_parm)
  })
  Clim_mod$Ann_Enviro_cn$HSI[,'Initial1']
  
  Clim_mod$Ann_Enviro_cn$Nit[,'Initial1']
  ### Step 9.7: Create lists for storing results for all models. ----------------
  
  Pop_results <- FUNparm(Clim_mod = Clim_mod,
                         Disp_parm = Disp_parm,
                         DMCombo = DMCombo)
  
  ### Step 9.(8+9): Run the FUNpopCalc function to calculate populations.--------------
  #### Initialize a progress bar for keeping track of progress
  progbar <- txtProgressBar(min = 1,
                            max = ncol(fields$HSI),
                            style = 3)
  ### Run FUNpopcalc for the selected years.
  for (i in
      floor(Disp_parm$avAge - (Disp_parm$bins / 2) + Disp_parm$bins + 1) :
      ncol(fields$HSI)) {
    
    ## To run the population function for all combinations for a single year:
    #i = 1950
    ## A short script to print a progress bar to the console for your benefit.
    setTxtProgressBar(progbar, i)
    
    ## Name the object that the results of the loop will be stored in.
    Pop_results <- lapply(Pop_results, function(x){
      
      ## To try it manually: 
      #x <- Pop_results[[1]]
      # c <-  x[names(x)[names(x) %in% c('ParmSet') == FALSE]][[1]]
      
      ### Due the follow for all elements of the list except parameters
      x[names(x)[names(x) %in% c('ParmSet') == FALSE]] <-
        lapply(x[names(x)[names(x) %in% c('ParmSet') == FALSE]],
               function(c){
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


## Section 5: Run the whole function and view results. -------------------------
for (Species in HyDiaDParameter %>% pull(Lname)) {
  print(Species)
  ### Load the corresponding calibrated brt ----
  brtModel <- read_rds(paste0("brt/brt_output/", 
                              Species_BRT %>% 
                                filter(Lname == Species) %>% 
                                pull(BRT_RDS)))
  
  ## Optional: Can clear the workspace, but keep outside dfs that are needed
  #rm(list = setdiff(ls(), c('AAbasins', 'All_Basins', 'Enviro',
  #                         'outletDistanceMatrix', 'Survey', 'brtModel', 'Yr10_Ann')))
  
  ## Define list of parameters to input for functions ----
  # First need to define which species is being tested 
  # in order to correctly subset the survey results
  Disp_parm <- list(
    ## Currently not used in functions (but could be varied for predict.gbm)
    ## Kappa parameter from the survey
    #kpa = Survey[Species, 'kpa'],
    ## 'tc' parameter from the survey
    #tc = Survey[Species, 'tc'],
    ## 'lr' parameter from the survey
    #lr = Survey[Species, 'lr'],
    ## 'bf' parameter from the survey
    #bf = Survey[Species, 'bf'],
    
    ## Currently used in HSDM functions:
    ## Name of the current species
    Lname =  Species,
    ## gamma = proportion of emigrants
    gamma = HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(gamma),
    ## alpha = scale parameter for dispersal kernel  
    alpha = HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(alpha),
    ## beta = shape parameter for dispersal kernel
    beta = HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(beta), 
    ## Sdisp = survival applied only to emigrants
    Sdisp =  HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(Sdisp),  
    ## Dmax = Maximal Density (number of spawners per km2)
    Dmax =  HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(Dmax), 
    ## lambda parameter related to Allee effect
    lambda =  HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(lambda),  
    ## r = population growth rate, without taking into account anthropogenic mortality
    r =  HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(r),
    ## Specify the average generation time for the species (yrs).
    avAge = HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(AgeFirstMat), # Whole numbers and decimals accepted (ie: 2 or 2.5).
    ## Specify the number of years to split offspring among (max < avAge*2).
    bins = HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(nbCohorts),
    ## DistMean : mean distance of straying (km)
    DistMean = HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(DistMean),
    
    ## Specify the rcp from enviro
    rcp = Enviro$Ann_Enviro_cn$rcp[1],
    ## eh1 = anthropogenic mortality related to habitat degradation; currently set to  1 
    eh1 = exp(0),
    ## eh2 =  anthropogenic mortality related to fish (e.g fishery); currently set to 1
    eh2 = exp(0),
    ## Set the timeperiod to run the model for
    envYr = c(1951:2100),
    
    ## Include an withAllee effect?
    withAllee = HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(withAllee),
    ## Include "accidental" straying to natal catchment?
    withNatalStray = HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(withNatalStray),
    ## Use presence/absence data when initializing populations?
    usePresence = HyDiaDParameter %>% filter(Lname == !!Species) %>% pull(usePresence)
  )
  
  ## Run the whole function ----
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
  
  write_rds(results, file = paste0("data_output/", suffix, "Results_", Species,"_",rcp, ".RDS" ))
}
## NOTE 2: Script to subset the results and create heatplots is in file "results_complexhp.R"




