## --------------------------- #
##
## Script name: HyDiaD,  Hybrid Species Distribution Model for Diadromous fish
##
## Purpose of script: This function calibrates a BRT with presence/absence of shads (from Eurodiad database) and environmental data  
## to estimate  habitat suitability in catchments. The BRT are used in HyDiaD simulation
##
## Author: Dr. Betsy Barber, 
##  Modified by Patrick Lambert
## Date Created: 2020-09-21
## Date Updated: 2022-02-02
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

library(dismo)
library(readxl)
library(magrittr)
library(tidyverse)

species = 'Alosa'
path_input = 'brt/brt_input/'
path_output = paste0('brt/brt_output/', species, '/')

if (!dir.exists(path_output))
  dir.create(path_output)

# laod data
All_Basins <- read_rds(paste0(path_input, "Info_All_Basins.RDS"))
load(paste0(path_input, "eurodiad_enviro_15march.Rdata"))

#Now need to get annual values:
Ann_Enviro <- eurodiad_enviro %>%
  group_by(year, basin_id) %>%
  summarise_at(vars(air_temperature:mixed_layer_depth), mean, na.rm = TRUE)
### rename columns for merging later
colnames(Ann_Enviro) <- c("year", "basin_id", "ann_air_temp", "ann_precip", "ann_SST", "ann_salinity", "ann_mix_depth")


### First I need to load the RDS file for a species and select only the data I want: 
## Change this each time I change species, but the rest should be ok
All <- read_rds(paste0(path_input, paste0(species, "_abund_AllYears.RDS")))
#All_Fallax <- read_RDS(paste0(path_input, "/Fallax_abund_AllYears.RDS"))

All_Data3 <- All
# filter(basin_id != 387)


brtFunc <- function(tc, lr, n.folds, bf){
  ### Now I need to organize abundance df to only include 1850-1951
  PA1900 <- All_Data3 %>%
    filter(year_from == '1851')
  
  Yr10_Ann <- Ann_Enviro %>%
    filter(year <= 1911) %>%
    group_by(basin_id) %>%
    summarise_at(vars(ann_air_temp:ann_mix_depth), mean, na.rm = TRUE)
  
  
  ### Merge dataframes to prepare for brt:
  All_Data2 <- merge(PA1900, All_Basins, "basin_id", sort = TRUE, all.x = TRUE)
  All_Data <- merge(All_Data2, Yr10_Ann, "basin_id", sort = TRUE, all.x = TRUE)
  
  ### remove the Nile
  # All_Data <- All_Data %>%
  #   filter(basin_id != 267)
  
  ### Create df to list all NAs:
  All_Data_na <- All_Data %>%
    filter_at(vars(Surf:ann_mix_depth), any_vars(is.na(.)))
  ### gbm won't accept NaN in predictor variables; need to change to "NA"
  is.nan.data.frame <- function(x){
    do.call(cbind, lapply(x, is.nan)) }
  All_Data[is.nan(All_Data)] <- NA
  
  ### Make sure that columns are correct format:
  #str(All_Data)
  All_Data$presence_absence <- as.numeric(All_Data$presence_absence)
  All_Data$ecoregion_name <- as.factor(All_Data$ecoregion_name)
  
  ### Initial brt
  require(dismo)
  scenarios <- c("Alt", "Surf", "Length", "ann_salinity", "ann_SST", "ann_precip", "ann_mix_depth")
  set.seed(1)
  tiff(paste0(path_output, "Base model holdout deviance.tiff"))
  base_model <- gbm.step(data = All_Data,
                         gbm.x = scenarios, 
                         gbm.y = 4, 
                         family = "bernoulli",
                         tree.complexity = tc,
                         learning.rate = lr,
                         bag.fraction = bf,
                         prev.stratify = TRUE, 
                         verbose = TRUE, 
                         plot.main = TRUE,
                         n.folds = n.folds) 
  dev.off()
  base_model_results <- base_model
  
  ### Save holdout deviance figure:
  
  ### Look at results
  tiff("Base model summary.tiff")
  summary(base_model)
  dev.off()
  sum_results <- base_model$contributions
  
  #Boxplot of fitted values:
  tiff(paste0(path_output, "Base model boxplot fitted values.tiff"))
  boxplot(base_model$fitted~base_model$data$y, xlab = "Annual", ylab = "Fitted values")
  dev.off()
  
  tiff(paste0(path_output, "Base model fitted functions.tiff"))
  gbm.plot(base_model, smooth = TRUE, rug = TRUE, write.title = TRUE)
  dev.off()
  
  n.drops = 5
  tiff(paste0(path_output, "simplified model predictive deviance.tiff"))
  simp_model <- gbm.simplify(base_model, n.drops = n.drops)
  dev.off()
  
  ### Is there a negative mean value in deviance summary?
  
  v4 <- simp_model$deviance.summary %>%
    mutate(order = c(1:5))
  
  #v5 <- v4[which(v4$mean < 0),]
  lowest2 <- min(v4$mean)
  #lowest <- min(v5$mean)
  v6 <- dplyr::filter(v4, v4$mean == lowest2)
  
  #lowest2 <- 0.1567
  ifelse(lowest2 < 0, scenarios2 <- as.vector(simp_model$pred.list[v6$order]), scenarios2 <- list(scenarios))
  
  
  #v3 <- simp_model$deviance.summary
  
  #lowest <- min(simp_model$deviance.summary$mean
  #ifelse (lowest < 0, scenarios2 , scenarios2 <- scenarios)
  
  #neg <- simp_model$deviance.summary$mean < 0
  
  
  tiff(paste0(path_output, "simplified model holdout deviance.tiff"))
  set.seed(1)
  base_model_simplified <- gbm.step(data = All_Data,
                                    gbm.x = scenarios2[[1]], 
                                    gbm.y = 4, 
                                    family = "bernoulli",
                                    tree.complexity = tc,
                                    learning.rate = lr,
                                    bag.fraction = bf,
                                    prev.stratify = TRUE, 
                                    verbose = TRUE, #controls screen reporting when running model
                                    plot.main = TRUE,
                                    n.folds = n.folds) 
  dev.off()
  
  simplified_model_results <- base_model_simplified
  
  tiff(paste0(path_output, "simplified model summary relative influence.tiff"))
  summary(base_model_simplified)
  dev.off()
  simp_sum_results <- base_model_simplified$contributions
  
  tiff(paste0(path_output, "simplified model boxplot fitted values.tiff"))
  boxplot(base_model_simplified$fitted~base_model_simplified$data$y, xlab = "Annual", ylab = "Fitted values")
  dev.off()
  
  tiff(paste0(path_output, "simplified model fitted functions.tiff"))
  gbm.plot(base_model_simplified, smooth = TRUE, rug = TRUE, write.title = TRUE)
  dev.off()
  
  ## Need original P/A vector, and fitted values from model run:
  indices <- data.frame(origPA = All_Data$presence_absence, fitted = base_model_simplified$fitted)
  
  ### This counts up the number of presences and absences IN THE TEST DATA SET, not the predicted data set
  #so for redo, this is counting up number of P and A in the original data set, not the fitted values?
  pres <- indices[indices[,1] == 1, 2]
  abs <- indices[indices[,1] == 0, 2]
  
  e <- evaluate(p = pres, a = abs)
  
  ### Good to record 6 plots:
  tiff(paste0(path_output, "evaluate results.tiff"))
  par(mfrow = c(2,3))
  plot(e, 'ROC') 
  plot(e, 'kappa') #Use this to find max kappa for threshold for rounding PREDICTED values to calculate overall kappa and TSS
  plot(e, 'FPR') #false positive rate
  boxplot(e, col = c('blue', 'red'))
  plot(e, 'FNR') #false negative rate
  plot(e, 'MCR') #misclassified rate
  dev.off()
  
  par(mfrow = c(1,1))
  
  ### Calculate threshold value for later use:
  kapdf <- data.frame(kappa = e@kappa, th = e@t)
  maxKappa <- max(kapdf$kappa)
  td <- dplyr::filter(kapdf, kapdf$kappa == maxKappa)
  threshold <- td$th
  
  ### First look at misclassification in model that will be USED FOR prediction (so this is from fitted values of base_model_simplified)
  
  #Use this for update (21.03) WITH THE MAX KAPPA VALUES TO GET PREDICTION ERROR!! 
  orig <- data.frame(Orig.PA = All_Data$presence_absence, Base.fitted = base_model_simplified$fitted)
  orig <- orig %>%
    mutate(fitted.PA = 
             ifelse(Base.fitted >= threshold, 1, 0)) %>% #syntax: ifelse(condition, do_if_true, do_if_false); use threshold at max kappa
    mutate(diff = Orig.PA - fitted.PA)
  
  orig.PE <- orig %>%
    dplyr::filter(diff < 0 | diff > 0)
  
  #Prediction error
  PredErr <- length(orig.PE$diff) / length(orig$Base.fitted)
  
  ### To calculate kappa and TSS ---------------------------------------------
  #False absences
  c <- sum(orig.PE$diff == 1)
  #False presences
  b <- sum(orig.PE$diff == -1)
  #True absences
  d <- sum(orig$Orig.PA == 0 & orig$fitted.PA == 0)
  #True presences
  a <- sum(orig$Orig.PA == 1 & orig$fitted.PA == 1)
  n <- length(orig$Orig.PA)
  
  
  ### Can calculate indices with excel file or with these functions:
  kappaIndex <- function(a, b, c, d, n){
    (
      ((a + d) / n) - 
        (
          (
            ((a + b)*(a + c)) + 
              ((c + d)*(d + b)) ) / 
            n^2)
    ) / 
      (1 - 
         (( ((a + b)*(a + c)) +
              ((c + d)*(d + b)) ) / 
            n^2)
      )
    
  }
  
  
  Kappa <- kappaIndex(a, b, c, d, n)
  tssIndex <- function(a, b, c, d){
    (a*d - b*c) / ((a + c)*(b + d))
  } 
  TSS <- tssIndex(a, b, c, d)
  
  Index_results <- list(PredErr = PredErr, FA = c, FP = b, TA = d, TP = a, N = n, Kappa = Kappa, TSS = TSS)
  
  return(list(base_model = base_model_results, drops = simp_model, simplified_model = simplified_model_results, evaluate = e, Index = Index_results))
}


# Alosa2 <- brtFunc(tc = 2, lr = 0.005, bf = 0.7, n.folds = 10)
Alosa1 <- brtFunc(tc = 1, lr = 0.005, bf = 0.7, n.folds = 10)

# Fallax2 <- brtFunc(tc = 2, lr = 0.005, bf = 0.7, n.folds = 10)
Fallax1 <- brtFunc(tc = 1, lr = 0.005, bf = 0.7, n.folds = 10)


write_rds(Fallax1, paste0(path_input, "Fallax_tc1_NoNile.RDS"))
write_rds(Alosa1, paste0(path_input, "Alosa_tc1_NoNile.RDS"))

