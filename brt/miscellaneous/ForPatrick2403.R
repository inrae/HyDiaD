### libraries:
library(dismo) # for gbm.step
library(gbm) # for predict.gbm

### Base model:
set.seed(1)
base_model <- gbm.step(data = tmpScen,
                       gbm.x = scenarios, #includes all appropriate variables 
                       gbm.y = 4,
                       family = "bernoulli",
                       tree.complexity = 2,  #includes first-order interactions
                       learning.rate = 0.005,
                       bag.fraction = 0.7,
                       prev.stratify = TRUE, 
                       verbose = TRUE, #controls screen reporting when running model
                       plot.main = TRUE,
                       n.folds = 10) 



### prediction: this is part of a loop that calculates results for each time step:
HSI[,i+1] <- predict.gbm(base_model, tmpScen, n.trees = base_model$gbm.call$best.trees, type = "response")

### tmpScen is a dataframe that includes basin_id, basin_name, presence_absence, surface area, length, altitude, precipitation, SST, 
### salinity, and mixed depth for each year i in the environmental data
