library(dismo)
library(tidyverse)
library(xlsx)
library(magrittr)

#Look for missing basins:--------------------------------------
#load Geraldine's dataset:
sqlAlosa <- read.xlsx("Alosa_1751_all data.xlsx", sheetIndex = 2, header = TRUE, as.data.frame = TRUE)
#load sturio dataset to compare which basins are missing environ pred variables:
GerSturio <- read.xlsx("modele_sturio.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)

#Find which basins are missing in GerSturio
a <- GerSturio$Basin
b <- sqlAlosa$Basin
#c <- as.data.frame(a[!(a %in% b)])
#I want this one, as it matters which one you are using first:
d <- as.data.frame(b[!(b %in% a)])
d <- as.character(d$Basin)

#Create dataframe for missing basins:
miss <- sqlAlosa %>%
  filter(Basin %in% c(d))
#write missing basins df to excel file:
write.xlsx(miss, "missing pred var alosa.xlsx")

#Add missing data if needed:-------------------------------------

#Load dataset with missing AA catchments
AddAA <- read.xlsx("AddBasinsAA.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)
#remove marine province column b/c will add later
AddAA <- AddAA %>%
  select(-3)
#Load names of all AA basins for filtering
#nom <- read.xlsx("Basins_AA_only.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)
#nom2 <- as.character(nom$Basins)

#First create file that has only basin information; all p/a dataframes can be joined with it by basin name for further analyses
#Remove lat, long, and marine province columns from Geraldine's dataset:
GerAA <- mydata %>% 
  select(c(1, 4, 6:12))
#filter rows so only have catchments in the AA:
#GerAA <- GerAA %>%
# filter(Basin %in% c(nom2))

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
ecode <- read.xlsx("All_basins_ecoregion.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)
#Join column of AA ecoregions to data for AA catchments; also calculate log of slope:
AllAA <- AllAA %>%
  left_join(ecode, by = "Basin") %>%
  mutate(log_slope = log(Slope))

#Write excel file that has all predictor variables for just AA catchments (for future reference):
write.xlsx(AllAA, "All_pred_var_sturio.xlsx")

#Now can load species-specific P/A data, select only AA catchments, and bind to predictor variables df for further analysis---------------------------
#Sturio:
#Load database of P/A data from 1751 to 1850:
#nom <- read.xlsx("Basins_AA_only.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)
#nom2 <- as.character(nom$Basins)
predv <- read.xlsx("All_pred_var_sturio.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)
nom2 <- as.character(predv$Basin)
sturiodb <- read.xlsx("Sturio_1751_all data.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)
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
Alosa <- sqlAlosa %>%
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
sqlAlosa <- read.xlsx("Alosa_1751_all data.xlsx", sheetIndex = 2, header = TRUE, as.data.frame = TRUE)
predvar <- read.xlsx("All_pred_var_sturio.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)

nom <- as.character(predvar$Basin)
Alosa <- sqlAlosa %>%
  filter(Basin %in% nom) %>%
  select(c(2, 9)) %>%
  left_join(predvar, by = "Basin")

str(Alosa)


GerSturio <- read.xlsx("modele_sturio.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)
nom2 <- as.character(GerSturio$Basin)
Alosa2 <- sqlAlosa %>%
  filter(Basin %in% nom2) %>%
  select(c(2, 9)) %>%
  left_join(predvar, by = "Basin")

#Combine predictor variables w/ P/A data 1850---------------------------------
sqlAlosa18 <- read.xlsx("Alosa_1851_all data.xlsx", sheetIndex = 2, header = TRUE, as.data.frame = TRUE)
predvar <- read.xlsx("All_pred_var_sturio.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)

nom <- as.character(predvar$Basin)
Alosa18 <- sqlAlosa18 %>%
  filter(Basin %in% nom) %>%
  select(c(2, 9)) %>%
  left_join(predvar, by = "Basin")

str(Alosa18)

#Set up scenarios to be tested------------------
scenarios <- cbind(rep("log_surf", 9), rep("MarPro", 9), rep("Slope", 9),(c(rep("TempAnn", 3), rep("TempSum", 3), rep("TempWin", 3))), 
                   (c(rep(c("PrecAnn", "PrecSum", "PrecWin"), 3))))

# Make sure that scenario names match what is in data table for every species:
scenarios2 <- c("log_slope", "TempAnn", "TempSum", "TempWin", "PrecAnn", "PrecSum", "PrecWin", "ecoregion_name")

scenarios3 <- c("log_slope", "TempAnn", "TempSum", "TempWin", "PrecAnn", "PrecSum", "PrecWin")

#Run for each possible combination of scenarios:------------------------------------
tc <- c(1:3) #define range of number of trees to test
lr <- c(0.01, 0.005, 0.001) #define range of learning rate to test
tmp <- 3 #Needed to make the loop store data properly, is equal to the number of values in lr

sum(Alosa18$presence_absence)

#Loop to run models for combination of tc and lr, and store output in nested list:
Scen1 <- list()
list1 <- list()
list2 <- list()
for (i in tc){
  #Scen1 <- list()
  #test1 <- list()
  set.seed(1)
  test1 <- list()
  list3 <- list()
  list4 <- list()
  for (b in 1:tmp){
    
    acisturio.Scen1.base <- gbm.step(data=Alosa18,
                                     gbm.x = scenarios3, #scenarios[1,], 
                                     gbm.y = 2, 
                                     family = "bernoulli",
                                     tree.complexity = i,
                                     learning.rate = lr[b],
                                     bag.fraction = 0.7,
                                     prev.stratify = TRUE, 
                                     verbose = TRUE, #controls screen reporting when running model
                                     plot.main = TRUE) #controls if plot is made when model is run 
    
    test1[[b]] <- acisturio.Scen1.base
    Scen1[[i]]<- test1
    list3[[b]] <- test1[[b]]$self.statistics
    list4[[b]] <- test1[[b]]$cv.statistics
    list1[[i]] <- list3
    list2[[i]] <- list4
  }
}

capture.output(list1, file = "initial_test_selfstats_Alosa.txt")
capture.output(list2, file = "initial_test_cvstats_Alosa.txt")
#function to create panelled plots of results to see the holdout deviance for each combination of tr and lr-------------
#To manually create plots of holdout deviance:
#set par to be panelled according to number of tc and lr tested:
plottc <- length(tc)
par(mfrow = c(plottc, tmp))
#plot results from model runs above:
results <- list()
for (i in tc){
  NumTree <- vector(mode = "numeric", length = tmp)
  for (c in 1:tmp){
    
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
    abline(v = target.trees, col=3)
    lines(trees.fitted, cv.loss.values + cv.loss.ses, lty=2)
    lines(trees.fitted, cv.loss.values - cv.loss.ses, lty=2)
    title(paste("P/A, tc - ", tc[i],", lr - ",lr[c], ", trees - ", target.trees, sep=""))
    
    #Create df for results:
    NumTree[c] <- target.trees
  }
  #bind dataframes together
  TRComp <- rep(i, length(tmp))
  settings <- cbind(TRComp, lr, NumTree)
  results[[i]] <- settings
}
AllRes <- as.data.frame(results)

#Now want to pull out summary results for each iteration-------------------------------------------
#Keep par so can graph vert bar chart w/ relative influence
#Also code so writes prop on bar
sum2 <- list()
for (i in tc){
  sum1 <- list()
  for (c in 1:tmp){
    summary(Scen1[[i]][[c]])
    title(paste("P/A, tc - ", tc[i],", lr - ",lr[c], sep=""))
    sum1[[c]] <- Scen1[[i]][[c]]$contributions
    sum2[[i]] <- sum1
  }
}

capture.output(sum2, file = "initial_test_summary_results_alosa1850_noecoregion.txt")
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
#Can try and use return() with the loop this time, it may be better
n.drops = 6
simple <- list()  
for (i in tc){
  simp1 <- list()
  for (c in 1:tmp){
    acisturio.simp <- gbm.simplify(Scen1[[i]][[c]], n.drops = n.drops)
    simp1[[c]] <- acisturio.simp
    simple[[i]] <- simp1
  }
}

#To make the plots to analyze whether variables should be dropped:
#Create plot for each panel:
for (i in tc){
  for (c in 1:tmp){
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

capture.output(simple, file = "simplify_results_alosa1850.txt")

#Now run final model after simplification-----------------------------------------------------------
#If any variables are dropped, use $pred.list[[number of variables dropped]]
#create list to hold gbm objects that have best "settings" (this has to be done manually for each )

#Need to do this for each set of scenarios
#For example, for the first scenario set for sturgeon I will use the model with lr = 0.01 and tc = 2, which gives me 1850 trees 
#Run model 10x with different seed set each time - get average values from the 10 model runs to calculate response curves
source("C:/Users/betsy.barber/Work Folders/Documents/Niche models/gbm.plot2.R")
#acisturio.Scen1.final <- Scen1[[2]][[2]]
nruns = 1
Scen1.final <- list()
lr = 0.01
tc = 2
scenarios3 <- unlist(simple[[1]][[1]]$pred.list[1])

par(mfrow = c(3, 3))
for (i in 1:nruns){
  set.seed(i)
  acisturio.Scen1.final <- gbm.step(Alosa18,
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


#Now need to make predictions to future temp data:---------------------------------
#Load in new data set for projections:
projtest <- read.xlsx("projection_A1F1_2050_test.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)

projtest <- projtest %>%
  dplyr::arrange(Nom_bassin_versant)

colsp = c(2, 4, 6:8)
projtest[,colsp] <- apply(projtest[, colsp], 2, 
                          function(x) as.numeric(as.character(x)))

#Run projection - need to create loop for this
pred.acisturio.test <- predict(Scen1.final[[3]], projtest, n.trees = Scen1.final[[3]]$gbm.call$best.trees, type = "response")
calc.deviance(obs = projtest$Presence_absence_1800, pred = pred.acisturio.test, calc.mean = TRUE)
d <- cbind(projtest$Presence_absence_1800, pred.acisturio.test)
pres <- d[d[,1] == 1, 2]
abs <- d[d[,1] == 0, 2]
e <- evaluate(p = pres, a = abs)
e

#These results show that there are still 68 presences and 128 absences, but some have changed from presence to absence and vice versa
#Still need to calculate kappa or TSS as index of model evaluation!!

#Not needed------------------------------------------------------
c(rep(1, 3), rep(2, 3), rep(3, 3))
c(rep(tc[1], 3), rep(tc[2], 3), rep(tc[3], 3))
cv.loss.values <- apply(Scen1[[1]][[1]]$cv.loss.matrix, 2, mean)
cv.loss.ses <- Scen1[[1]][[1]]$cv.loss.ses
y.bar <- min(cv.loss.values)
trees.fitted <- Scen1[[1]][[1]]$trees.fitted
y.min <- min(cv.loss.values - cv.loss.ses) 
y.max <- max(cv.loss.values + cv.loss.ses) 
plot(Scen1[[1]][[1]]$trees.fitted, cv.loss.values, type = 'l', ylab = "holdout deviance",
     xlab = "no. of trees", ylim = c(y.min,y.max))
abline(h = y.bar, col = 2)
lines(trees.fitted, cv.loss.values + cv.loss.ses, lty=2)
lines(trees.fitted, cv.loss.values - cv.loss.ses, lty=2) 
target.trees <- trees.fitted[match(TRUE,cv.loss.values == y.bar)]
abline(v = target.trees, col=3)
title(paste("P/A, tc - ",tc,", lr - ",lr[1], sep=""))



summary(test1[[1]])

cv.loss.values <- apply(Scen1[[1]][[1]]$cv.loss.matrix,2,mean)


colnames(TotalEggs) <- c("Year", (lapply(1:ndams, function(x){paste0("TotalEggs",x)})))
for (b in 1:3){
  paste0("scen", b) <- list()
}
tmp <- 3
sapply(1:tmp, function(x) {test1[[x]] <- acisturio.tc2.lr005.base})



ex_fun <- function(arg1, arg2){
  col <- arg1 + arg2
  x <- as.data.frame(col)
}
arg1 <- seq(1, 10, by = 3)
arg2 <- seq(2, 11, by = 3)
df <- map2_dfr(arg1, arg2, ex_fun)
# If instead you want to bind by columns, use map2_dfc() or pmap_dfc()
df2 <- map2_dfc(arg1, arg2, ex_fun)


l2 <- list(
  list(num = 1:3,     letters[1:3]),
  list(num = 101:103, letters[4:6]),
  list()
)
l2 %>% map(c(2, 2))     




mean.delta <- apply(simple[[1]][[3]]$deviance.matrix, 1, mean)
se.delta <- sqrt(apply(simple[[1]][[3]]$deviance.matrix,1,var))/sqrt(n.folds)
y.max <- 1.5 * max(mean.delta + se.delta)
y.min <- 1.5 * min(mean.delta - se.delta)
y.min2 <- if (y.min < 0) y.min else 0
original.deviance.se <- round(Scen1[[1]][[3]]$cv.statistics$deviance.se,4)
#Plot for each panel:
plot(seq(0,n.drops),c(0,mean.delta),xlab="variables removed", ylab = "change in predictive deviance",type='l',ylim=c(y.min2,y.max))
lines(seq(0,n.drops),c(0,mean.delta) + c(0,se.delta),lty = 2)
lines(seq(0,n.drops),c(0,mean.delta) - c(0,se.delta),lty = 2)
abline(h = 0 , lty = 2, col = 3)
min.y <- min(c(0,mean.delta))
min.pos <- match(min.y,c(0,mean.delta)) - 1 # subtract one because now zero base
abline(v = min.pos, lty = 3, col = 2)
abline(h = original.deviance.se, lty = 2, col = 2)
title(paste("RFE deviance - PA - folds = ",n.folds, "tc - ", tc[i],", lr - ",lr[c], sep=""))

