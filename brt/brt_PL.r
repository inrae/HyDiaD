# work flow
# 1. calibration dataset
#   1.1 request from eurodiad
#   1.2. check whether environmental data are available
# 2. application dataset
#   2.1 request from eurodiad or CCM
#   2.2. check whether environmental data are available

library(dismo)
library(readxl)
library(magrittr)
library(tidyverse)

library(xlsx)

rm(list = ls())

# path to files
path_input = 'brt/brt_input/'
path_output = 'brt/brt_output/'
#Look for missing basins:--------------------------------------
#load Geraldine's dataset:
eurodiadAlosa <- read_xlsx(paste0(path_input,"Alosa_1751_all data.xlsx"), sheet = 2)
#load sturio dataset to compare which basins are missing environ pred variables:
GerSturio <- read_xlsx(paste0(path_input,"modele_sturio.xlsx"), sheet = 1)

#Find which basins are missing in GerSturio
eurodiadAlosa %>% 
  anti_join(GerSturio, by = 'Basin') %>% 
  dplyr::select(Basin)

#Add missing data if needed:-------------------------------------

#Load dataset with missing AA catchments
#remove marine province column b/c will add later
AddAA <- read_xlsx(paste0(path_input,"AddBasinsAA.xlsx"), sheet = 1) %>%
  select(-MarPro)

#Load names of all AA basins for filtering
nom <- read_xlsx(paste0(path_input,"Basins_AA_only.xlsx"), sheet = 1)

#First create file that has only basin information; all p/a dataframes can be joined with it by basin name for further analyses
#Remove lat, long, and marine province columns from Geraldine's dataset:
GerAA <- GerSturio %>% 
  select(c(1, 4, 6:12)) 

#filter rows so only have catchments in the AA:
# filter(Basin %in% c(nom))

#Convert columns to numeric:
cl <- c(2:9)
GerAA[,cl] <- apply(GerAA[, cl], 2, function(x) as.numeric(as.character(x)))
AddAA[,cl] <- apply(AddAA[, cl], 2, function(x) as.numeric(as.character(x)))

#Bind by rows Geraldine's dataset and the missing AA catchments:
AllAA <- GerAA %>%
  bind_rows(AddAA)

#make sure we don't have any repreating rows
un <- AllAA %>%
  distinct()

#Load file with ecoregion codes for AA catchments:
ecode <- read_xlsx(paste0(path_input,"All_basins_ecoregion.xlsx"), sheet = 1)
#Join column of AA ecoregions to data for AA catchments; also calculate log of slope:
AllAA <- AllAA %>%
  left_join(ecode, by = "Basin") %>%
  mutate(log_slope = log(Slope))

#Write excel file that has all predictor variables for just AA catchments (for future reference):
write.xlsx(AllAA, paste0(path_input,"All_pred_var_sturio.xlsx"))

#Now can load species-specific P/A data, select only AA catchments, and bind to predictor variables df for further analysis---------------------------
#Sturio:
#Load database of P/A data from 1751 to 1850:
#nom <- read.xlsx("Basins_AA_only.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)
#nom2 <- as.character(nom$Basins)
predv <- read_xlsx(paste0(path_input,"All_pred_var_sturio.xlsx"), sheet = 1)
nom2 <- as.character(predv$Basin)
sturiodb <- read_xlsx(paste0(path_input,"Sturio_1751_all data.xlsx"), sheet = 1)
#Filter by AA catchment names, then select only the P/A data, then join with predictor variables:
sturio <- sturiodb %>%
  filter(Basin %in% c(nom2)) %>%
  select(c(2, 9)) %>%
  left_join(predv, by = "Basin") %>%
  select (-3)

#Check that all rows had a match between the two data tables
#notsturio <- sturio %>%
#anti_join(sturiodb, by = "Basin")

#Make sure P/A in numeric, along with other predictor variables, ecoregion code is factor, and basin is ?character?
str(sturio)

#Clean data------------------------------------
#Select basins where pred var data is available:
nom3 <- as.character(GerSturio$Basin)
GerSturio2 <- GerSturio %>%
  select(1, 6:12)
Alosa <- eurodiadAlosa %>%
  filter(Basin %in% nom3) %>%
  select(c(2, 9, 11)) %>%
  left_join(GerSturio2, by = "Basin")

#Make sure data structure is correct:
str(Alosa)
cols = c(2, 4:10)
Alosa[,cols] <- apply(Alosa[, cols], 2, function(x) as.numeric(as.character(x)))

Alosa <- Alosa %>%
  mutate(log_slope = log(Slope))


#Combine predictor variables with P/A data 1750---------------------------------
sqlAlosa <- read_xlsx(paste0(path_input, "Alosa_1751_all data.xlsx"), sheet = 2)
predvar <- read_xlsx(paste0(path_input, "All_pred_var_sturio.xlsx"), sheet = 1)

nom <- as.character(predvar$Basin)
Alosa <- sqlAlosa %>%
  filter(Basin %in% nom) %>%
  select(c(2, 9)) %>%
  left_join(predvar, by = "Basin")

str(Alosa)


GerSturio <- read_xlsx(paste0(path_input, "modele_sturio.xlsx"), sheet = 1)
nom2 <- as.character(GerSturio$Basin)
Alosa2 <- sqlAlosa %>%
  filter(Basin %in% nom2) %>%
  select(c(2, 9)) %>%
  left_join(predvar, by = "Basin")

#Combine predictor variables w/ P/A data 1850---------------------------------
sqlAlosa18 <- read_xlsx(paste0(path_input, "Alosa_1851_all data.xlsx"), sheet = 2)
predvar <- read.xlsx(paste0(path_input, "All_pred_var_sturio.xlsx"), sheet = 1)

nom <- as.character(predvar$Basin)
Alosa18 <- sqlAlosa18 %>%
  filter(Basin %in% nom) %>%
  select(c(2, 9)) %>%
  left_join(predvar, by = "Basin") %>% 
  mutate(log_surf = log(Surf))

str(Alosa18)

## Set up scenarios of predictors variables to be tested -----------------
# Make sure that scenario names match what is in data table for every species:
#
scenarios1 <- as.matrix(expand.grid("log_surf",
            "ecoregion_name",
            "Slope",
            c("TempAnn", "TempSum", "TempWin" ),
            c("PrecAnn", "PrecSum", "PrecWin")))
# 
# scenarios2 <- c("log_slope", "TempAnn", "TempSum", "TempWin", "PrecAnn", "PrecSum", "PrecWin", "ecoregion_name")

scenarios3 <- c("log_slope", "TempAnn", "TempSum", "TempWin", "PrecAnn", "PrecSum", "PrecWin")

#Run for each possible combination of scenarios:------------------------------------
treeComplexities <- c(1:3) #define range of number of trees to test
learningRates <- c(0.01, 0.005, 0.001) #define range of learning rate to test

sum(Alosa18$presence_absence)
# ======================================================================================= #
# Loop to run models for combination of tree complexity and learning, and store output in nested list:
# TODO precise what are the lists
# CHECK ootion var.monotone = rep(0, length(gbm.x)) in gbm
Scen1 <- list()
list1 <- list()
list2 <- list()
for (tc in treeComplexities) {

  set.seed(1)
  test1 <- list()
  list3 <- list()
  list4 <- list()
  for (lr in 1:length(learningRates)) {
    cat('tc=', tc, ' lr=', learningRates[lr], '\n')
    brt_result <- gbm.step(data = Alosa18 %>% as.data.frame(),
                                     gbm.x = scenarios3,  #  scenarios1[1,],
                                     gbm.y = 'presence_absence', 
                                     family = "bernoulli",
                                     tree.complexity = tc,
                                     learning.rate =  learningRates[lr],
                                     bag.fraction = 0.7,
                                     prev.stratify = TRUE, 
                                     verbose = FALSE, #controls screen reporting when running model
                                     plot.main = FALSE) #controls if plot is made when model is run 
    
    test1[[lr]] <- brt_result
    Scen1[[tc]] <- test1
    list3[[lr]] <- brt_result$self.statistics
    list4[[lr]] <- brt_result$cv.statistics
    list1[[tc]] <- list3
    list2[[tc]] <- list4
  }
}

capture.output(list1, file = paste0(path_output,"initial_test_selfstats_Alosa.txt"))
capture.output(list2, file = paste0(path_output,"initial_test_cvstats_Alosa.txt"))


# ================================================================================================================ #
#function to create panelled plots of results to see the holdout deviance for each combination of tr and lr-------------
#To manually create plots of holdout deviance:
#set par to be panelled according to number of treeComplexities and learningRates tested:

par(mfrow = c(length(treeComplexities), length(learningRates)) )
#plot results from model runs above:
results <- list()
for (i in treeComplexities){
  NumTree <- vector(mode = "numeric", length = length(learningRates))
  for (c in 1:length(learningRates)) {
    
    #For each panel, calculate parameters:
    cv.loss.values <- apply(Scen1[[i]][[c]]$cv.loss.matrix, 2, mean)
    cv.loss.ses <- Scen1[[i]][[c]]$cv.loss.ses
    y.bar <- min(cv.loss.values)
    trees.fitted <- Scen1[[i]][[c]]$trees.fitted
    target.trees <- trees.fitted[match(TRUE,cv.loss.values == y.bar)]
    y.min <- min(cv.loss.values - cv.loss.ses) 
    y.max <- max(cv.loss.values + cv.loss.ses)
    
    
    #Create plot for each panel:
    plot(trees.fitted, cv.loss.values, type = 'l', ylab = "holdout deviance",
         xlab = "no. of trees", ylim = c(y.min,y.max))
    abline(h = y.bar, col = 2)
    abline(v = target.trees, col = 3)
    lines(trees.fitted, cv.loss.values + cv.loss.ses, lty=2)
    lines(trees.fitted, cv.loss.values - cv.loss.ses, lty=2)
    title(paste("P/A, tc - ", treeComplexities[i],", lr - ",learningRates[c], ", trees - ", target.trees, sep=""))
    
    #Create df for results:
    NumTree[c] <- target.trees
  }
  #bind dataframes together
  TRComp <- rep(i, length(learningRates))
  settings <- cbind(TRComp, lr, NumTree)
  results[[i]] <- settings
}
AllRes <- as.data.frame(results)

#Now want to pull out summary results for each iteration-------------------------------------------
#Keep par so can graph vert bar chart w/ relative influence
#Also code so writes prop on bar
sum2 <- list()
for (i in treeComplexities){
  sum1 <- list()
  for (c in 1:length(learningRates)){
    summary(Scen1[[i]][[c]])
    title(paste("P/A, tc - ", tc[i],", lr - ",lr[c], sep=""))
    sum1[[c]] <- Scen1[[i]][[c]]$contributions
    sum2[[i]] <- sum1
  }
}

capture.output(sum2, file = paste0(path_output,"initial_test_summary_results_alosa1850_noecoregion.txt"))
#tmp3 <- tmp * length(tc)

#Creates df to hold results if want to see in a table
AllSum <- as.data.frame(sum2)
AllSum2 <-  AllSum %>%
  select(contains("rel.inf"))
colnm <- paste(c(rep(1, 3), rep(2, 3), rep(3, 3)), c(0.05, 0.01, 0.005)) #I still need to soft code this
colnames(AllSum2) <- colnm

#Try modifying this for column names:
#dimnames(dev.results) <- list(paste("drop.",1:n.drops,sep=""), paste("rep.", 1:n.folds, sep=""))


#for (i in 1:tmp3){
# tc1 <-  tc
#lr1 <- lr
#colnm <- (sprintf("tc %g lr %g", tc1, lr1))
#}



#one <- lapply(tc, function(x){rep(x, length(tc))})
#two <- lapply(lr, function(x){rep(x, length(lr))})
#paste((lapply(tc, function(x){c(x, length(tc))})), lr, collapse = )



#With tc of 4 and 5 and lr of 0.05, was told to restart the model with a smaller learning rate or smaller step size


#Now need to write loop for gbm.simplify-----------------------------------------------------------
#     takes an inital cross-validated model as produced by gbm.step 
#     and then assesses the potential to remove predictors using k-fold cross validation. 
#Can try and use return() with the loop this time, it may be better
n.drops = 6 # number of drops to check
simple <- list()  
for (i in treeComplexities){
  simp1 <- list()
  for (c in 1:length(learningRates)){
    acisturio.simp <- gbm.simplify(Scen1[[i]][[c]], n.drops = n.drops)
    simp1[[c]] <- acisturio.simp
    simple[[i]] <- simp1
  }
}

#To make the plots to analyze whether variables should be dropped:
#Create plot for each panel:
for (i in treeComplexities){
  for (c in 1:lenth(learningRates)){
    #Calculate parameters
    n.folds = 10
    n.drops = n.drops
    mean.delta <- apply(simple[[i]][[c]]$deviance.matrix, 1, mean)
    se.delta <- sqrt(apply(simple[[i]][[c]]$deviance.matrix,1,var))/sqrt(n.folds)
    y.max <- 1.5 * max(mean.delta + se.delta)
    y.min <- 1.5 * min(mean.delta - se.delta)
    y.min2 <- if (y.min < 0) y.min else 0
    original.deviance.se <- round(Scen1[[i]][[c]]$cv.statistics$deviance.se,4)
    #Plot for each panel:
    plot(seq(0,n.drops),c(0,mean.delta),xlab="variables removed", ylab = "change in predictive deviance",type='l',ylim=c(y.min2,y.max))
    lines(seq(0,n.drops),c(0,mean.delta) + c(0,se.delta),lty = 2)
    lines(seq(0,n.drops),c(0,mean.delta) - c(0,se.delta),lty = 2)
    abline(h = 0 , lty = 2, col = 3)
    min.y <- min(c(0,mean.delta))
    min.pos <- match(min.y,c(0,mean.delta)) - 1 # subtract one because now zero base
    abline(v = min.pos, lty = 3, col = 2)
    abline(h = original.deviance.se, lty = 2, col = 2)
    title(paste("RFE deviance - PA - folds = ",n.folds, ", tc - ", tc[i],", lr - ",lr[c], sep=""))
    
  }
}

capture.output(simple, file = paste0(path_output, "simplify_results_alosa1850.txt"))

#Now run final model after simplification-----------------------------------------------------------
#If any variables are dropped, use $pred.list[[number of variables dropped]]
#create list to hold gbm objects that have best "settings" (this has to be done manually for each )

#Need to do this for each set of scenarios
#For example, for the first scenario set for sturgeon I will use the model with lr = 0.01 and tc = 2, which gives me 1850 trees 
#Run model 10x with different seed set each time - get average values from the 10 model runs to calculate response curves
source("gbm.plot2.R")
#acisturio.Scen1.final <- Scen1[[2]][[2]]
nruns = 1
Scen1.final <- list()
lr = 0.01
tc = 2
scenarios3 <- unlist(simple[[1]][[1]]$pred.list[1])

par(mfrow = c(3, 3))
for (i in 1:nruns){
  set.seed(i)
  acisturio.Scen1.final <- gbm.step(Alosa18 %>% as.data.frame(),
                                    gbm.x = scenarios3,  #scenarios[1,], 
                                    gbm.y = 2, 
                                    family = "bernoulli",
                                    tree.complexity = tc,
                                    learning.rate = lr,
                                    bag.fraction = 0.7,
                                    prev.stratify = TRUE,
                                    verbose = FALSE, #controls screen reporting when running model
                                    plot.main = FALSE)
  Scen1.final[[i]] <- acisturio.Scen1.final
  
  #For each panel, calculate parameters:
  cv.loss.values <- apply(Scen1.final[[i]]$cv.loss.matrix, 2, mean)
  cv.loss.ses <- Scen1.final[[i]]$cv.loss.ses
  y.bar <- min(cv.loss.values)
  trees.fitted <- Scen1.final[[i]]$trees.fitted
  target.trees <- trees.fitted[match(TRUE,cv.loss.values == y.bar)]
  y.min <- min(cv.loss.values - cv.loss.ses) 
  y.max <- max(cv.loss.values + cv.loss.ses)
  
  #Create plot for each panel:
  plot(trees.fitted, cv.loss.values, type = 'l', ylab = "holdout deviance",
       xlab = "no. of trees", ylim = c(y.min,y.max))
  abline(h = y.bar, col = 2)
  abline(v = target.trees, col=3)
  lines(trees.fitted, cv.loss.values + cv.loss.ses, lty=2)
  lines(trees.fitted, cv.loss.values - cv.loss.ses, lty=2)
  title(paste("P/A, tc - ", tc,", lr - ",lr, ", trees - ", target.trees, sep=""))
  
  #Plot summary:
  summary(Scen1.final[[i]])
  title(paste("P/A, tc - ", tc,", lr - ",lr, sep=""))
  
  #plot fitted functions:
  gbm.plot2(Scen1.final[[i]], n.plots = length(scenarios3), write.title = TRUE)
  
  #plot empty plots so have all plots on one sheet -- need to soft code this!
  # plot(0,type='n',axes=FALSE, ann = FALSE)
  #plot(0,type='n',axes=FALSE, ann = FALSE)
  
  #Come up with some way to save the graph....
  #ggsave(filename = (paste("plots", tc, "lr", lr, "p", i, "tiff", sep = "." )), plot = last_plot(), device = "tiff",
  #path = "C:/Users/betsy.barber/Work Folders/Documents/Niche models/Plots")
  
  
}


#dev.copy(tiff, "myplot.tiff")
#gbm.plot(Scen1.final[[2]], n.plots = 5, write.title = TRUE)

dev.off()


# #Now need to make predictions to future temp data:---------------------------------
# #Load in new data set for projections:
# projtest <- read.xlsx("projection_A1F1_2050_test.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)
# 
# projtest <- projtest %>%
#   dplyr::arrange(Nom_bassin_versant)
# 
# colsp = c(2, 4, 6:8)
# projtest[,colsp] <- apply(projtest[, colsp], 2, 
#                           function(x) as.numeric(as.character(x)))
# 
# #Run projection - need to create loop for this
# pred.acisturio.test <- predict(Scen1.final[[3]], projtest, n.trees = Scen1.final[[3]]$gbm.call$best.trees, type = "response")
# calc.deviance(obs = projtest$Presence_absence_1800, pred = pred.acisturio.test, calc.mean = TRUE)
# d <- cbind(projtest$Presence_absence_1800, pred.acisturio.test)
# pres <- d[d[,1] == 1, 2]
# abs <- d[d[,1] == 0, 2]
# e <- evaluate(p = pres, a = abs)
# e
# 
# #These results show that there are still 68 presences and 128 absences, but some have changed from presence to absence and vice versa
# #Still need to calculate kappa or TSS as index of model evaluation!!
# 
# #Not needed------------------------------------------------------
# c(rep(1, 3), rep(2, 3), rep(3, 3))
# c(rep(tc[1], 3), rep(tc[2], 3), rep(tc[3], 3))
# cv.loss.values <- apply(Scen1[[1]][[1]]$cv.loss.matrix, 2, mean)
# cv.loss.ses <- Scen1[[1]][[1]]$cv.loss.ses
# y.bar <- min(cv.loss.values)
# trees.fitted <- Scen1[[1]][[1]]$trees.fitted
# y.min <- min(cv.loss.values - cv.loss.ses) 
# y.max <- max(cv.loss.values + cv.loss.ses) 
# plot(Scen1[[1]][[1]]$trees.fitted, cv.loss.values, type = 'l', ylab = "holdout deviance",
#      xlab = "no. of trees", ylim = c(y.min,y.max))
# abline(h = y.bar, col = 2)
# lines(trees.fitted, cv.loss.values + cv.loss.ses, lty=2)
# lines(trees.fitted, cv.loss.values - cv.loss.ses, lty=2) 
# target.trees <- trees.fitted[match(TRUE,cv.loss.values == y.bar)]
# abline(v = target.trees, col=3)
# title(paste("P/A, tc - ",tc,", lr - ",lr[1], sep=""))
# 
# 
# 
# summary(test1[[1]])
# 
# cv.loss.values <- apply(Scen1[[1]][[1]]$cv.loss.matrix,2,mean)
# 
# 
# colnames(TotalEggs) <- c("Year", (lapply(1:ndams, function(x){paste0("TotalEggs",x)})))
# for (b in 1:3){
#   paste0("scen", b) <- list()
# }
# tmp <- 3
# sapply(1:tmp, function(x) {test1[[x]] <- acisturio.tc2.lr005.base})
# 
# 
# 
# ex_fun <- function(arg1, arg2){
#   col <- arg1 + arg2
#   x <- as.data.frame(col)
# }
# arg1 <- seq(1, 10, by = 3)
# arg2 <- seq(2, 11, by = 3)
# df <- map2_dfr(arg1, arg2, ex_fun)
# # If instead you want to bind by columns, use map2_dfc() or pmap_dfc()
# df2 <- map2_dfc(arg1, arg2, ex_fun)
# 
# 
# l2 <- list(
#   list(num = 1:3,     letters[1:3]),
#   list(num = 101:103, letters[4:6]),
#   list()
# )
# l2 %>% map(c(2, 2))     
# 
# 
# 
# 
# mean.delta <- apply(simple[[1]][[3]]$deviance.matrix, 1, mean)
# se.delta <- sqrt(apply(simple[[1]][[3]]$deviance.matrix,1,var))/sqrt(n.folds)
# y.max <- 1.5 * max(mean.delta + se.delta)
# y.min <- 1.5 * min(mean.delta - se.delta)
# y.min2 <- if (y.min < 0) y.min else 0
# original.deviance.se <- round(Scen1[[1]][[3]]$cv.statistics$deviance.se,4)
# #Plot for each panel:
# plot(seq(0,n.drops),c(0,mean.delta),xlab="variables removed", ylab = "change in predictive deviance",type='l',ylim=c(y.min2,y.max))
# lines(seq(0,n.drops),c(0,mean.delta) + c(0,se.delta),lty = 2)
# lines(seq(0,n.drops),c(0,mean.delta) - c(0,se.delta),lty = 2)
# abline(h = 0 , lty = 2, col = 3)
# min.y <- min(c(0,mean.delta))
# min.pos <- match(min.y,c(0,mean.delta)) - 1 # subtract one because now zero base
# abline(v = min.pos, lty = 3, col = 2)
# abline(h = original.deviance.se, lty = 2, col = 2)
# title(paste("RFE deviance - PA - folds = ",n.folds, "tc - ", tc[i],", lr - ",lr[c], sep=""))

