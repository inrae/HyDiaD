library(readxl)
library(tidyverse)

rm(list = ls())
### Note: requires rdata distance matrix 'dmUse' ###
dmUse = readRDS("survey exploitation/dmUse.RDS")

## Need the 'Survey' df that stores the output from the expert survey for all 11 species
# Variables from the survey or FishBase and are stored in excel file:
## Set rownames to subset later
Survey <- read_xlsx("data_input/Species_Parameters.xlsx", sheet = 1, col_names = TRUE) %>%
  column_to_rownames('Lname')


## ---------------------------------------------------------  All fish emigrate ####
# reference points
y = 0.1629 # Proportion emigrants from survey
# For old dispersal kernel
dispersal_1 = data.frame(D = c(40.47, 255.51), pAfter = c(.5, .1))
dispersal_1$pBefore = 1 - dispersal_1$pAfter
# For including ALL fish in the dispersal kernel
dispersal_2 = data.frame(D = c(40.47, 255.51), pAfter = c(y * .5, y * .1))
dispersal_2$pBefore = 1 - dispersal_2$pAfter

# calibration by linear regression
lm1 = lm(log(-log(pAfter))~log(D), data = dispersal_1)
lm2 = lm(log(-log(pAfter))~log(D), data = dispersal_2)


# the kernel function
disperse = function(D, alpha, beta){
  return(exp(-alpha * (D^beta)))
}

# compute the propotion  to settle before a distance
# based on the integrale of the disperse function....
settleBefore = function(D, alpha, beta) {
  pPeudoInf = 10000 * pgamma( (10000^beta)*alpha, 1/beta)  / (beta * ((10000^beta * alpha)^(1 / beta)))
  res = (D * pgamma( (D^beta)*alpha, 1/beta)  / (beta * ((D^beta * alpha)^(1 / beta)))) / pPeudoInf
  res[D == 0] = 0
  return(res)
}

# sum of squared error
SCE = function(par, obsDispersal){
  return(sum((settleBefore(obsDispersal$D, par[1], par[2]) - obsDispersal$pBefore)^2))
}

# generate a set of distances
D = 0:1000

# Calculate a solution
sol_1 = optim(c(0.06, .652), SCE, obsDispersal = dispersal_1)
sol_2 = optim(c(0.06, .652), SCE, obsDispersal = dispersal_2)
# View the parameters
sol_1$par
sol_2$par

# Plot the instantaneous prob of settling
# Just emigrants
plot(D, disperse(D, exp(lm1$coefficients[1]), lm1$coefficients[2]),type = 'l', yaxp= c(0,1,10))
# Scaled to inclued the homers and the emigrants
lines(c(0, D), c(1,disperse(D, exp(lm1$coefficients[1]), lm1$coefficients[2])* y),
      type = 'l',)
# All fish are emigrants
lines(D, disperse(D, exp(lm2$coefficients[1]), lm2$coefficients[2]),type = 'l', col = 'red')

# Proportion of fish settled by the survey distances
settleBefore(dispersal_1$D, sol_1$par[1], sol_1$par[2]) * y + (1 - y)
settleBefore(dispersal_2$D, sol_2$par[1], sol_2$par[2])




### Make disperal matricies for different aplha and beta
## Calculate the relative fraction of fish that would return to each
## basin using the parameters "a" and "b".

Mat_EmgNO <- exp(-sol_1$par[1]*(dmUse^sol_1$par[2])) #Natal = no
Mat_EmgYes <- exp(-sol_1$par[1]*(dmUse^sol_1$par[2])) #Natal = YES
Mat_ALL <- exp(-sol_2$par[1]*(dmUse^sol_2$par[2])) #All fish

### If Natal Stray = 'no', this places a zero in the diagonal (l = j),
## so no fish 'stray' into their natal catchment.
diag(Mat_EmgNO) <- 0

### Divide all rows in a column of expMatrix2 by the sum of the column.
## creates an absoulute dispersal fraction from the relative fractions.
Mat_1 <- Mat_EmgNO / colSums(Mat_EmgNO)
Mat_2 <- Mat_EmgYes / colSums(Mat_EmgYes)
Mat_3 <- Mat_ALL / colSums(Mat_ALL)

## Create a dataframe with cumulative sums of fish dispersing from all basins.
dat1 <- data.frame(
  fish = c(Mat_1),
  dist = c(dmUse)
)
dat1 <- dat1[with(dat1, order(dist)), ]
dat1$Cum <- cumsum(dat1$fish) / nrow(Mat_1)

dat2 <- data.frame(
  fish = c(Mat_2),
  dist = c(dmUse)
)
dat2 <- dat2[with(dat2, order(dist)), ]
dat2$Cum <- cumsum(dat2$fish) / nrow(Mat_2)

dat3 <- data.frame(
  fish = c(Mat_3),
  dist = c(dmUse)
)
dat3 <- dat3[with(dat3, order(dist)), ]
dat3$Cum <- cumsum(dat3$fish) / nrow(Mat_3)


## Make a plot of various lines
# Plot the cumulative settlement of JUST the emigrants
plot(D, settleBefore(D, sol_1$par[1], sol_1$par[2]),
     type = 'l', yaxp= c(0,1,10), ylab = 'Proportion of fish settled',
     lwd = 1, ylim = c(0.0,1), xlim = c(0, 1000))

# Add lines for the distances
abline(v = dispersal_1$D, col = 'red')

# Use the same dispersal kernal, plot ALL fish emigrants and homers
lines(c(0, D),
      c(0, settleBefore(D, sol_1$par[1], sol_1$par[2]) * y + (1 - y)),
      type = 'l', lwd = 1)

# Use the new dispersal kernal, plot ALL fish
lines(D, settleBefore(D, sol_2$par[1], sol_2$par[2]),
      type = 'l', lwd = 1, col = 'red')

# Plot the empirical distribution of dispersal
# For seperate homing/emigrants, Natal stray = No
lines(x = c(0, dat1$dist),
      y = c(0, dat1$Cum * y + (1 - y)),
      col = 'blue', lwd = 2)
# For seperate homing/emigrants, Natal stray = Yes
lines(x = c(0, dat2$dist),
      y = c(0, dat2$Cum * y + (1 - y)),
      col = 'darkseagreen', lwd = 2)
# For ALL fish are emigrants, Natal stray = Yes
lines(x = c(0, dat3$dist),
      y = c(0, dat3$Cum),
      col = 'magenta', lwd = 2)
text(x = c(400, 400, 400, 400, 400), y = c(0.7, 0.6, 0.5, 0.4, 0.3),
     labels = c('Theoretical, All fish, Some Emigrate',
                'Theoretical, All fish, All Emigrate',
                'Empirical, All fish, All Emigrate',
                'Empirical All fish, Natal = Yes',
                'Empirical All fish, Natal = No'),
     col = c('black', 'red', 'magenta', 'darkseagreen', 'blue'))




