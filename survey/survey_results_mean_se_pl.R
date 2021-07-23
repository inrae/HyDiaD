## ---------------------------
##
## Script name: survey_results_mean_se.R
##
## Purpose of script: Calculate and Summarize Results from Diadromous Species Survey using CL for question and species
##
## Author: Dr. Betsy Barber
##
## Date Created: 2020-08-31
## Date Updated: 2020-12-22
##
## Copyright (c) Dr. Betsy Barber, 2020
## Email: betsy.barber@maine.edu
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
## Notes: This code lists the methods used to calculate the weighted mean and standard error for all diadromous species survey
##   questions. Results from some questions were log-transformed. Script for creating figures is also included.
##
## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

## Packages:  (uncomment as required)

library(readxl)

library(zoo)
library(viridis)
library(scales)
library(tidyverse)
## ---------------------------

## Sourced Functions:

source("QuantFigNotLog.R") 
source("QuantFigLogScale.R")

## ---------------------------------------------------------------------------------------------------------------
##setwd("C:/Users/Betsy/Desktop/HDSM/HSDM_Script_Full/HDSM_all_files/species_parameters")
#### SCRIPT IS ARRANGED IN STEPS:------------------------------
#### Step 1: Load the data and reshape ---------------------------------------------------
###Note: Change name of file and sheetIndex as needed, but need to specify columns are characters and stringsAsFactors is false in order to convert to numeric below

##First load confidence levels for each participant and question
path = "survey/data_input/"
CLRes <- read_xlsx(paste0(path, "Survey_Response_Final.xlsx"), sheet = 1)

## Next load the survey answer for each participant and question
Values <- read_xlsx(paste0(path, "Survey_Response_Final.xlsx"), sheet = 2)

## Melt df for confidence level from wide format to long format for working with tidy
## Replace instances of na with "0" - for confidence levels only
clres2 <- CLRes %>% 
  pivot_longer(cols = DJ:IK, names_to = 'Person', values_to = 'CL') %>% 
  replace_na(list(CL = 0))

## Melt df for responses from wide format to long format
## Replace instances of na with "0" - for confidence levels only
values2 <- Values %>% 
  pivot_longer(cols = DJ:IK, names_to = 'Person', values_to = 'Value') %>% 
  replace_na(list(Value = 0))

## Join confidence level and response dataframes by question, species, and participant (person)
df <- clres2 %>% 
  inner_join(values2, by = c('Question', 'Species', 'Person'))

# STOP HERE

##Note: generally, instances where confidence level = 0 should correspond with response = NA, but this may not always be the case
# if participants provided a response but listed their confidence in their response as 0

#### Step 2a Confidence Levels: Calculate mean, se for confidence levels for all questions ----------------------------------

## First find the number of participants to use in SE calculation:
nb <- clres2 %>% 
  n_distinct('Person')

## Calculate mean and SE for confidence levels:
# Here, "Response" is the indicated CL because we use the dataframe that only includes CL information 
SppConf <- clres2 %>%
  mutate_at("Response", as.numeric) %>%
  dplyr::select(-Person) %>%
  drop_na(Response) %>% #drop na's
  filter(Response > 0) %>% #only include instances with CL > 0
  group_by(Species) %>%
  summarise_at("Response", c(sum = sum, mean = mean, sd = sd)) %>% #calculate sum, mean, and sd for CL
  mutate(lowerSD = mean - sd, upperSD = mean + sd) %>%
  mutate(lowerSE = mean - (sd / sqrt(np)), upperSE = mean + (sd / sqrt(np))) #calculate se from sd

# Caculate the number of responses for each species
SppConfZero <- clres2 %>%
  mutate_at("Response", as.numeric) %>%
  dplyr::select(-Person) %>%
  filter(Response != 0) %>%
  group_by(Species) %>%
  count(Species)

SppConf <- inner_join(SppConf, SppConfZero, by = "Species")


#### Step 2b Confidence Levels: Create figures of confidence levels and respondents by region ---------------------------------
### Respondents by region:

## Load in data with region listed for each participant
Respondents <- read.xlsx("Respondents_by_region.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE, 
                         colClasses = (Response = "character"), stringsAsFactors = FALSE)

# Select all unique combinations of person, region, and species: 
Region2 <- values2 %>%
  drop_na(Response) %>%
  distinct(Person, Species, .keep_all = T) %>%
  inner_join(Respondents, by = "Person") %>%
  select(Person, Species, region)

# Count the number of responses for each species
BySpecies <- Region2 %>%
  count(Species) %>%
  mutate_at("n", as.numeric) %>%
  arrange(n)
RankBS <- as.vector(BySpecies$Species)
BySpecies$Species <- factor(BySpecies$Species, levels = RankBS)

#First show total number of participants for each species:
ggplot(data = BySpecies, aes(fill = Species, x = factor(Species, level = RankBS), y = n)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = n), size = 6, vjust = -1) +
  ggtitle("Number of Participants for Each Species (23 Total Participants)") +
  theme(legend.position = "none", axis.text.x = element_text(size = 16, angle = 75, vjust = 0.6, hjust = 0.5),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18), 
        title = element_text(size = 18), strip.text.x = element_text(size = 16)) +
  xlab("Species") + 
  ylab("Number of Participants")


## Then divide this by region:
# Rank species by number or participants for gridded plot
Rank3 <- Region2 %>%
  arrange(factor(Species, levels = RankBS))
Rank4 <- as.vector(Rank3$Species)
Region2$Species <- factor(Region2$Species, levels = RankBS)

# Define labels
RgLb <- setNames(paste(unique(BySpecies$Species), "\nTotal N:", BySpecies$n), unique(BySpecies$Species))

# Rank regions within each plot
Region2$region <- factor(Region2$region, levels = c("Portugal", "Spain", 
                                                    "France", "UK", "Ireland"))

#Create figure
ggplot(data = Region2, aes(fill = region, x = region)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_viridis_d(option = "E", end = 0.9) +
  ggtitle("Species Covered in Each Region") +
  facet_wrap(~Species,
             labeller = labeller(Species = setNames(unlist(RgLb), unique(BySpecies$Species)))) +
  theme(legend.position = "none", axis.text.x = element_text(size = 16, angle = 90, vjust = 0.6, hjust = 0.5),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18), 
        title = element_text(size = 18), strip.text.x = element_text(size = 16)) +
  xlab("Region") + 
  ylab("Number of Participants")


### Note: If creating several figures with a different change order for the factors, if don't define the color scheme explicitly,
## the colors will be associated with the order and not a particular species
## You can set a color scale at the beginning that is linked to the levels in a variable (i.e. species)
# myColors <- viridis::magma(n = 11)
# Confid$Species <- factor(Confid$Species, levels = c("Allis Shad","Twaite Shad",
#                                                    "River Lamprey", "Sea Lamprey", 
#                                                    "Sea Trout", "Salmon", 
#                                                    "Smelt", "Sturgeon",
#                                                    "Flounder", "Mullet",  "Eel"))
# names(myColors) <- levels(Confid$Species)
# Use this color command for continuous data:
# colScale <- scale_colour_manual(name = "Species", values = myColors)
# Use this color command for discrete data:
# fillScale <- scale_fill_manual(name = "Species", values = myColors)


### Confidence levels 

## Set level for CL responses for use in figures
clres2$Response <- as.character(clres2$Response) #if set to numeric, doesn't work
Confid3 <- clres2 %>%
  drop_na(Response)
Confid3$Response <- factor(Confid3$Response, levels = c("0", "25", "50", "75", "100"))

## Drop NA, calculate sum
# This uses counts from "BySpecies", as defined in section for respondents by region (Line 126)
ConfTotal <- clres2 %>%
  drop_na(Response) %>%
  group_by(Species) %>%
  mutate_at("Response", as.numeric) %>%
  summarise_at("Response", sum) %>%
  inner_join(BySpecies, by = "Species")

# Write function for plots
CLFunc <- function(data1, data2){
  clb <- setNames(paste(unique(data2$Species), "\nTotal CL:", data2$Response, " N:", data2$n), unique(data2$Species))
  ggplot(data = data1, aes(fill = Response, x = Response)) +
    geom_bar(position = "dodge", color = "black") +
    scale_fill_viridis_d(option = "E") +
    ggtitle("Confidence levels by Species") +
    facet_wrap(~Species,
               labeller = labeller(Species = setNames(unlist(clb), unique(data2$Species)))) +
    theme(legend.position = "none", axis.text = element_text(size = 16), 
          axis.title = element_text(size = 18), 
          title = element_text(size = 18), strip.text.x = element_text(size = 16)) +
    xlab("Confidence Level") + 
    ylab("Number of Responses (out of 176 total)")
}

# First organize plots by species group
SppList <- c("Allis Shad","Twaite Shad",
             "River Lamprey", "Sea Lamprey", 
             "Sea Trout", "Salmon", 
             "Smelt", "Sturgeon",
             "Flounder", "Mullet",  "Eel")
clres2$Species <- factor(clres2$Species, levels = SppList)
ConfTotal$Species <- factor(ConfTotal$Species, levels = SppList)
# Create plots
CLFunc(Confid3, ConfTotal) + ggtitle("Confidence Level Ranked by Species Groups")

## Next organize plots by rank (low confidence to high confidence) - also write total confidence next to species name

# Define function for ranking:
RankFunc <- function(name1, data4, name2){
  name1 <- data4 %>%
    group_by(Species) %>%
    mutate_at("Response", as.numeric) %>%
    summarize_at("Response", sum) %>%
    arrange(Response)
  RankCL <- as.vector(name1$Species)
  return(RankCL)
}

CLRank <- RankFunc(name1 = Confid2, data4 = clres2, name2 = RankCL)

# Rank results
Confid4 <- clres2 %>%
  drop_na(Response)
Confid4$Species <- factor(Confid4$Species, levels = CLRank)
Confid4$Response <- factor(Confid4$Response, levels = c("0", "25", "50", "75", "100"))

# Create figure that ranks by total confidence level, low to high 
CLFunc(Confid4, ConfTotal) + ggtitle("Confidence Level Ranked by Total Value Low to High")

## Next show mean and sd for each species:
# organize by species grouping, then by low to high
SppGrps <- c("Allis Shad","Twaite Shad","River Lamprey", "Sea Lamprey", "Sea Trout", 
             "Salmon", "Smelt", "Sturgeon","Flounder", "Mullet",  "Eel")

# Use dataframes calculated in Step 2a:
SppConf$Species <- factor(SppConf$Species, levels = SppGrps)

SppConf <- SppConf %>%
  arrange(mean)
scrank <- as.vector(SppConf$Species)
SppConf <- SppConf %>%
  arrange(factor(Species, levels = scrank)) 

#This creates plot of mean CL by species with SE added as error bars
ggplot(data = SppConf, aes(x = factor(Species, level = scrank), y = mean, fill = Species)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = lowerSE, ymax = upperSE), width = 0.2) +
  ylim(0, 100) +
  geom_text(aes(label = round(mean)), size = 6, vjust = 6) + 
  ggtitle("Mean and Standard Error of Confidence Levels (Black = Mean)") + #, 
  theme(legend.position = "none", axis.text.x = element_text(size = 18, angle = 75, vjust = 0.5, hjust = 0.5), 
        axis.title = element_text(size = 18), axis.text.y = element_text(size = 16),
        title = element_text(size = 18)) +
  xlab("Species") + 
  ylab("Mean of Confidence Levels (\U00B1 SE)")

#### Step 3a Quantitative Questions: Calculate weighted average and define functions for SD ----------------------
### Calculate weighted means using CL for each question and person combination:
## Weighted frequency is sum of all values for each species
## These calculatons are only for questions 3, 4, 5, 6, 7, and 8; Q2 is calculated separately in another section

## These answers need to be log-transformed
temp3 <- c(3, 4, 6, 7)
## These answers are percentages and do not need to be log-transformed
temp4 <- c(5, 8)
## Need separate lists and combined list fot the following code
temp5 <- c(temp3, temp4)

### Drop NAs for individual people and species combinations and calculate the response X confidence level column
## log-transform responses as necessary, and multiply response * CL rowwise for each question and participant
AllData <- df %>%
  filter(Question %in% temp5) %>% #specify list of all quantitative questions
  drop_na(Value) %>% # drop any instance where the participant did not answer one of the questions (which happened often)
  mutate_at(c("Value", "CL"), as.numeric) %>%
  mutate(mResponse = case_when(
    Question == 4 ~ 1/Value, #question 4 was calculated differently as 1/response
    Question != 4 ~ Value
  )) %>%
  mutate(
    lResponse = case_when(
      Question %in% temp3 ~ log(mResponse), #log-transform questions in list temp3
      Question %in% temp4 ~ mResponse)) %>% #keep the rest of the questions untransformed (in list temp4)
  mutate(ResxCon = mResponse * CL) %>% 
  mutate(CompResCon = lResponse * CL) #multiply indiviual response (log transformed as necessary) by individual CL

#str(AllData)

### Summarise data for each question and species to get sum of response and sum of confidence, then calc weighted average
## Also define functions to calculate weighted SD (use bias-corrected formula)

## Summarize data and calculate weighted average for each question and species (both on log scale and not on log scale)
SumAllData <- AllData %>%
  drop_na(ResxCon)%>%
  filter(CL > 0) %>%
  group_by(Question, Species) %>%
  summarise_at(c("CL", "ResxCon", "CompResCon"), sum) %>%
  mutate(
    WtAve = case_when(
      Question %in% temp3 ~ (CompResCon / CL), #for questions on log scale, do not exponentiate yet
      Question %in% temp4 ~ ResxCon / CL)) #for questions not on log scale, I shouldn't need this anymore b/c CompResCon is both logged and not logged


## Set up a dataframe to store counts for multiple groupings, for use in calculating standard deviation and for use in figures
AllList <- list()
AllCnt <- list()
for(i in temp5){ #For each question in the list of quantitative questions defined above
  temp6 <- AllData %>% #This is used to define the points in the figures
    filter(Question == i) %>% #for each question
    drop_na(ResxCon)%>% #drop NAs
    filter(CL > 0) %>% #only include responses where the CL was > 0
    count(Question, Species, CL, mResponse, name = "N") #Count the number of participants that gave the same CL and value for a question and species
  temp8 <- AllData %>% #This is used to calculate the weighted mean and SD
    filter(Question == i & CL > 0) %>%
    count(Question, Species) #Count the total number of responses for each question and species
  AllList[[i]] <- temp6
  AllCnt[[i]] <- temp8
}

### For the SD, calculate the number of samples with CL > 0 that will be used in the equation as N
## Join the individual responses to the summarized data
AllT <- inner_join(AllData, SumAllData, by = c("Question", "Species"))

### This dataframe is used to calculate n for the survey questions that are NOT on a log scale
## This calculates the number of responses for each question and species
N <- AllT %>%
  filter(CL.x > 0) %>%
  drop_na(Value) %>%
  group_by(Species) %>%
  count(Question, Species) %>%
  mutate_at("n", as.numeric)
## Add N to the summarized dataframe
SumAllData2 <- inner_join(SumAllData, N, by = c("Question", "Species"))


### Calculate the weighted standard deviation in two steps, with separate functions for questions on a log scale and not on a log scale

## For questions that ARE on a log scale:

# Step A: Calculate first part of equation and effective N (Neff)
SDSumLog <- function(i){
  AllT %>%
    filter(Question == i & CL.x > 0) %>%
    drop_na(Value) %>%
    group_by(Species) %>%
    mutate(xma = (lResponse - (WtAve))^2) %>% #(xi - u*)^2
    mutate(wxma = xma * CL.x, sqConf = (CL.x^2)) %>% #(wi * (xi - u*)^2)
    summarise_at(c("wxma", "CL.x", "sqConf"), sum) %>%
    mutate(SumConfsq = CL.x^2) %>%
    mutate(Neff = SumConfsq / sqConf)
}

# Step B: Use effective N to calculate standard error and standard deviation
expfunc <- function(x){exp(x)}
SDfuncLog <- function(i, Q){
  SumAllData %>%
    filter(Question == i) %>%
    inner_join(Q, SumAllData, by = c("Species")) %>%
    mutate(S2 = (Neff/((Neff-1) *CL.x)) * wxma) %>%
    mutate(S = (sqrt(S2/Neff))) %>% 
    mutate(upSD = exp(WtAve + S)) %>% #exponentiate upper and lower SD for final value
    mutate(dnSD = exp(WtAve - S)) %>% 
    mutate_at("WtAve", expfunc) #exponentiate weigted average for final value
  
}

## For questions that are NOT on a log scale:

#Step A: Calculate first part of equation
SDSumNoLog <- function(i){
  AllT %>%
    filter(Question == i & CL.x > 0) %>%
    drop_na(Value) %>%
    group_by(Species) %>%
    mutate(xma = (mResponse - (WtAve))^2) %>% #(xi - u*)^2
    mutate(wxma = xma * CL.x) %>% #(wi * (xi - u*)^2)
    summarise_at(c("wxma", "CL.x"), sum) #sum wi*(xi-u*^2) and sum of wi
}

#Step B: calculate standard error and standard deviation using n (count of responses for each question and species)
SDfuncNoLog <- function(i, Q){
  SumAllData2 %>%
    filter(Question == i) %>%
    inner_join(Q, SumAllData, by = c("Species")) %>%
    mutate(S2 = (n/((n-1) *CL.x)) * wxma) %>%
    mutate(S = (sqrt(S2))) %>% 
    mutate(upSD = (WtAve + S)) %>%
    mutate(dnSD = (WtAve - S))
}

#### Step 3b Quantitative Questions: Use the functons defined above to calculate weighted SD for each question, then create figures to show results-----------------------------------------
## Questions 2 (fish density) and 3 (Allee effect) are at the end of this section because they required additional steps

#### Q4: Natural survival from egg to adult; on log scale -----------------------------
## Calculate SD
Q4 <- SDSumLog(4) 
Q42 <- SDfuncLog(4, Q4) 
## Arrange results for use in figures
Q4Spp <- Q42 %>% 
  group_by(Species) %>%
  arrange(WtAve)
RankQ4 <- as.vector(Q4Spp$Species)
AllList[[4]]$Species <- factor(AllList[[4]]$Species, levels = RankQ4 )
AllCnt[[4]]$Species <- factor(AllCnt[[4]]$Species, levels = RankQ4)
Q42$Species <- factor(Q42$Species, levels = RankQ4)
## Create figure
QuantFigLogScale(AllList[[4]], Q42, AllCnt[[4]]) + ggtitle("Natural Survival (1 adult / N eggs)")
## Save results
Q42 <- data.frame(Q42)
# write.xlsx(Q42, "Survey results by question and species_updated Nov 2020.xlsx",
#           sheetName = "Q4")


#### Q5: Proportion of emigrant fish; not on log scale --------------------------------------------
## Calculate SD:
Q5 <- SDSumNoLog(5)
Q52 <- SDfuncNoLog(5, Q5)
## Arrange results for figure:
Q5Spp <- Q52 %>%
  group_by(Species) %>%
  arrange(WtAve)
RankQ5 <- as.vector(Q5Spp$Species)
AllList[[5]]$Species <- factor(AllList[[5]]$Species, levels = RankQ5)
AllCnt[[5]]$Species <- factor(AllCnt[[5]]$Species, levels = RankQ5)
Q52$Species <- factor(Q52$Species, levels = RankQ5)
## Create figure:
QuantFigNotLog(AllList[[5]], Q52, AllCnt[[5]]) + ggtitle("Proportion of emigrant fish (%)") + 
  scale_x_continuous(breaks = c(5, 10, 20, 50, 80, 90, 95, 100)) + 
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 0.5, vjust = 0.5))
## Save results
Q52 <- data.frame(Q52)
# write.xlsx(Q52, "Survey results by question and species_updated Nov 2020.xlsx",
#           sheetName = "Q5", append = TRUE)


#### Q6: Mean distance that emigrant disperses; on log scale ----------------------------------------------------
## Calculate SD:
Q6 <- SDSumLog(6)
Q62 <- SDfuncLog(6, Q6)
## Arrange results for figure:
Q6Spp <- Q62 %>%
  group_by(Species) %>%
  arrange(WtAve)
RankQ6 <- as.vector(Q6Spp$Species)
AllList[[6]]$Species <- factor(AllList[[6]]$Species, levels = RankQ6)
AllCnt[[6]]$Species <- factor(AllCnt[[6]]$Species, levels = RankQ6)
Q62$Species <- factor(Q62$Species, levels = RankQ6)
## Create figure:
QuantFigLogScale(data1 = AllList[[6]], data2 = Q62, 
                 data3 = AllCnt[[6]]) + ggtitle("Mean distance emigrant disperses (km)") +
  scale_x_continuous(trans = "log10",
                     breaks = c(1, 10, 100, 1000),
                     labels = c("1", "10", "100", "1 000"))
## Save results:
Q62 <- data.frame(Q62)
# write.xlsx(Q62, "Survey results by question and species_updated Nov 2020.xlsx",
#           sheetName = "Q6", append = TRUE)



#### Q7: Maximum distance an emigrant disperses; on log scale -------------------------------------------------------
## Calculate SD:
Q7 <- SDSumLog(7)
Q72 <- SDfuncLog(7, Q7)
## Arrange results for figure:
Q7Spp <- Q72 %>%
  group_by(Species) %>%
  arrange(WtAve)
RankQ7 <- as.vector(Q7Spp$Species)
AllList[[7]]$Species <- factor(AllList[[7]]$Species, levels = RankQ7)
AllCnt[[7]]$Species <- factor(AllCnt[[7]]$Species, levels = RankQ7)
Q72$Species <- factor(Q72$Species, levels = RankQ7)
## Create figure:
QuantFigLogScale(AllList[[7]], Q72, AllCnt[[7]]) + ggtitle("Max distance emigrant disperses (km)") +
  scale_x_continuous(trans = "log10",
                     breaks = c(10, 100, 1000, 2500),
                     labels = c("10", "100", "1 000", "> 1 000"))
## Save results:
Q72 <- data.frame(Q72)
# write.xlsx(Q72, "Survey results by question and species_updated Nov 2020.xlsx",
#           sheetName = "Q7", append = TRUE)


## For questions 6 and 7, make sure that results for Q6 are smaller than results for Q7 and display visually:
Diff6 <- Q62 %>%
  select(Question, Species, WtAve, upSD, dnSD)
Diff7 <- Q72 %>%
  select(Question, Species, WtAve, upSD, dnSD)
Diff67 <- inner_join(Diff6, Diff7, by = "Species") %>%
  mutate(Diff = WtAve.y - WtAve.x) %>%
  arrange(Diff)

### Show difference of Q7 minus Q6 for each species using bar chart; difference should be > 0
ggplot() +
  geom_col(data = Diff67, aes(x = Diff, y = Species, fill = Species), color = "black") +
  geom_vline(data = Diff67, aes(xintercept = 0), lty = 3, col = "black", lwd = 1) +
  theme(legend.position = "none", axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  xlab("Difference (Maximum - Mean distance)") + scale_color_viridis_d(option = "A")


#### Q8: Emigrant survival; not on log scale ----------------------------------------------------------------
## Calculate SD:
Q8 <- SDSumNoLog(8)
Q82 <- SDfuncNoLog(8, Q8)
## Arrange for figure:
Q8Spp <- Q82 %>%
  group_by(Species) %>%
  arrange(WtAve)
RankQ8 <- as.vector(Q8Spp$Species)
AllList[[8]]$Species <- factor(AllList[[8]]$Species, levels = RankQ8)
AllCnt[[8]]$Species <- factor(AllCnt[[8]]$Species, levels = RankQ8)
Q82$Species <- factor(Q82$Species, levels = RankQ8)
## Create Figure:
QuantFigNotLog(AllList[[8]], Q82, AllCnt[[8]]) + ggtitle("Emigrant survival (%)") + 
  scale_x_continuous(breaks = c(1, 10, 50, 90),
                     labels = c("1", "10", "50", "90"))
## Save results:
Q82 <- data.frame(Q82)
# write.xlsx(Q82, "Survey results by question and species_updated Nov 2020.xlsx",
#           sheetName = "Q8", append = TRUE)


## To save results from multiple questions together in one file:
# ResultsLog <- do.call("rbind", list(Q32, Q42, Q62, Q72))
# write.csv(x = ResultsLog, file = "Results for logged questions.csv")
# ResultsNoLog <- do.call("rbind", list(Q52, Q82))
# write.csv(x = ResultsNoLog, file = "Results for non logged questions.csv")


#### Q2: Maximum fish density; requires additional calculations ----------------------------
### The results for this question are stored in a different file in two separate pages:
## The first page has the responses, with density already calculated using the surface area for the provided rivers
# Surface area was taken from the EuroDiad database
# A future update could use the surface area of estimated available spawning habitat rather than the entire watershed
Q2 <- read.xlsx("Survey_response_Q2.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE, 
                colClasses = (Response = "character"), stringsAsFactors = FALSE)
## The second page has the confidence levels
clq2 <- read.xlsx("Survey_response_Q2.xlsx", sheetIndex = 2, header = TRUE, as.data.frame = TRUE,
                  colClasses = (Response = "character"), stringsAsFactors = FALSE)

## Organize response dataframe in long format:
ResultsQ2 <- gather(Q2, Person, Response, AW:RK)

## Only need density for calculations:
DQ2 <- ResultsQ2 %>%
  filter(Category == "Density")

## Organize confidence level dataframe in long format, remove any instances with no response, and replace remaining NAs with 0:
clq2 <- gather(clq2, Person, Response, DJ:RK)
clq2 <- clq2 %>%
  drop_na(Species) %>%
  replace_na(list(Response = 0)) %>%
  rename_at("Response", ~"Conf")

##Combine the dataframes
SurResp <- inner_join(DQ2, clq2, by = c("Species", "Person"))

## Drop NAs, calculate log of response, and multiply by confidence level
AllDataQ2 <- SurResp %>%
  drop_na(Response) %>%
  mutate_at(c("Response", "Conf"), as.numeric) %>%
  mutate(lResponse = log(Response)) %>%
  mutate(ResxCon = lResponse * Conf)

## Calculate weighted mean
SumAllDataQ2<- AllDataQ2 %>%
  drop_na(ResxCon)%>%
  filter(Conf > 0) %>%
  group_by(Species) %>%
  summarise_at(c("Conf", "ResxCon"), sum) %>%
  mutate(WtAve = ResxCon / Conf)

## Count individual response combinations and total responses for all species (for figures)
AllList2 <- AllDataQ2 %>%
  drop_na(ResxCon)%>%
  filter(Conf > 0) %>%
  count(Species, Conf, Response, name = "N")
AllCnt2 <- AllDataQ2 %>%
  filter(Conf > 0) %>%
  count(Species)

## Combine individual responses with summarized dataframe
AllT2 <- inner_join(AllDataQ2, SumAllDataQ2, by = "Species")

#Add column for counts to dataframe
N2 <- AllT2 %>%
  filter(Conf.x > 0) %>%
  drop_na(Response) %>%
  group_by(Species) %>%
  count(Species) %>%
  mutate_at("n", as.numeric)

SumAllDataQ22 <- inner_join(SumAllDataQ2, N2, by = c("Species"))

## Step A for calculating the weighted SD using unbiased estimates (with Neff)
SDSumQ2 <- AllT2 %>%
  filter(Conf.x > 0) %>%
  drop_na(Response) %>%
  group_by(Species) %>%
  mutate(xma = (lResponse - (WtAve))^2) %>% #(xi - u*)^2
  mutate(wxma = xma * Conf.x, sqConf = (Conf.x^2)) %>% #(wi * (xi - u*)^2)
  summarise_at(c("wxma", "Conf.x", "sqConf"), sum) %>%
  mutate(SumConfsq = Conf.x^2) %>%
  mutate(Neff = SumConfsq / sqConf)

## Step B define function to calculate weighted SD, then exponentiate weighted mean and SD
SDfuncQ2 <- SumAllDataQ22 %>%
  inner_join(SDSumQ2, SumAllDataQ2, by = c("Species")) %>%
  mutate(S2 = (Neff/((Neff-1) *Conf.x)) * wxma) %>%
  mutate(S = (sqrt(S2/Neff))) %>% 
  mutate(upSD = exp(WtAve + S)) %>%
  mutate(dnSD = exp(WtAve - S)) %>%
  mutate_at("WtAve", expfunc)

## Apply function to data for Q2
Q2Spp <- SDfuncQ2 %>%
  group_by(Species) %>%
  arrange(WtAve)
## Arrange results for figure
RankQ2 <- as.vector(Q2Spp$Species)
AllList2$Species <- factor(AllList2$Species, levels = RankQ2)
AllCnt2$Species <- factor(AllCnt2$Species, levels = RankQ2)
SDfuncQ2$Species <- factor(SDfuncQ2$Species, levels = RankQ2)

## Create figure
Q2Fig <- function(data1, data2, data3){
    lbls <- setNames(paste(unique(data2$Species), ";Total CL:", data2$Conf, ";N:", data3$n,
                         #"\nMean:", round(data2$WtAve), "+", round(data2$upSD), ",-", round(data2$dnSD),
                         #"SE"), unique(data2$Species))
                         "\nMean:", scientific(data2$WtAve, digits = 2), "+", scientific(data2$upSD, digits = 1),
                         ",-", scientific(data2$dnSD, digits = 1),"SE"), unique(data2$Species))
  ggplot() +
    geom_point(data = data1, aes(x = Response, y = Conf, size = N, fill = N),
               alpha = 0.5, shape = 21, color = "black", stroke = 1.5) +
    geom_vline(data = data2, aes(xintercept = WtAve), lty = 2, col = "blue", lwd = 1) +
    geom_rect(data = data2, aes(xmin = dnSD, xmax = upSD, ymin = 0, ymax = 100), color = "lightblue", alpha = 0.25) +
    facet_wrap(~Species,
               labeller = labeller(Species = setNames(unlist(lbls), unique(data1$Species)))) +
    theme_bw() +
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), 
          title = element_text(size = 18), strip.text.x = element_text(size = 16)) +
    ylim(0, 100) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)) +
    scale_size_continuous(range = c(9, 12), breaks = pretty_breaks(4)) +
    scale_fill_viridis(option = "D", breaks = pretty_breaks(4)) +
    guides(fill = guide_legend(), size = guide_legend()) +
    ylab("Confidence Level") +
    xlab("Answer Response (log scale)") 
}

Q2Fig(AllList2, SDfuncQ2, AllCnt2) + ggtitle("Maximum Density (fish / km2)")

SppList <- c("Allis Shad","Twaite Shad",
             "River Lamprey", "Sea Lamprey", 
             "Sea Trout", "Salmon", 
             "Smelt", "Sturgeon",
             "Flounder", "Mullet",  "Eel")

## Save results
Q2Spp <- data.frame(Q2Spp)
# write.xlsx(Q2Spp, "Survey results by question and species_updated Nov 2020.xlsx",
#           sheetName = "Q2", append = TRUE)



#### Q3: Allee effect; requires additional calculations -------------------------------------------------------
### Need to use SA in calcuation of results

## Load surface area information
Q3SA <- read.xlsx("Survey_Response_Final.xlsx", sheetIndex = 3, header = TRUE, as.data.frame = TRUE,
                  colClasses = (Response = "character"), stringsAsFactors = FALSE)

## Organize data in long format
Q3_values <- gather(Q3SA, Person, SA, DJ:PL)
Q3_values <- Q3_values %>%
  select(Question, Species, Person, SA)

## combine with response values from previously created dataframe (df)
Q3_join <- inner_join(df, Q3_values, by = c("Question", "Species", "Person"))

### To calculate lambda, need results from Q2 (maximum density)
## Can either re-run script for Q2 or load the results from excel file:
Survey <- read.xlsx("Species_Results_Survey.xlsx", sheetIndex = 2, header = TRUE, as.data.frame = TRUE)
## Select columns that will be used
Dmax_values <- data.frame(Species2 = Survey$Lname, Dmax = Survey$Dmax, 
                          Species = c("Allis Shad", "Twaite Shad", "River Lamprey", 
                                      "Flounder", "Salmon", "Sturgeon", "Eel", "Sea Trout",
                                      "Sea Lamprey", "Smelt", "Mullet"))
Dmax_values$Dmax <- as.numeric(Dmax_values$Dmax)
## Combine Dmax and responses from Q3
df2 <- inner_join(Q3_join, Dmax_values, by = "Species")

## Drop NAs, calculate lambda, convert to log scale, multiply by confidence level
Q3_indi <- df2 %>%
  drop_na(Value) %>%
  drop_na(SA) %>%
  mutate_at(c("Value", "CL", "SA"), as.numeric) %>%
  mutate(lambda = Value / (Dmax * SA)) %>%
  mutate(loglambda = log(Value / (Dmax * SA))) %>%
  mutate(LxCL = loglambda * CL) 

expfunc <- function(x){exp(x)}

#str(AllData)

## Calculate weighted average
SumAllDataQ3<- Q3_indi %>%
  filter(CL > 0) %>%
  group_by(Species) %>%
  summarise_at(c("CL", "LxCL"), sum) %>%
  mutate(WtAve = LxCL / CL) 

## Get counts for figures
AllList3 <- Q3_indi %>%
  filter(CL > 0) %>%
  count(Species, CL, Value, name = "N")
AllCnt2 <- Q3_indi %>%
  filter(CL > 0) %>%
  count(Species)

AllT2 <- inner_join(Q3_indi, SumAllDataQ3, by = "Species")

## Get counts for calculation of SD
N3 <- AllT2 %>%
  group_by(Species) %>%
  count(Species) %>%
  mutate_at("n", as.numeric)

SumAllDataQ32 <- inner_join(SumAllDataQ3, N3, by = c("Species"))

## Step A of standard deviation calculation
SDSumQ3log <- AllT2 %>%
  filter(CL.x > 0) %>%
  group_by(Species) %>%
  mutate(xma = (loglambda - (WtAve))^2) %>% #(xi - u*)^2
  mutate(wxma = xma * CL.x, sqConf = (CL.x^2)) %>% #(wi * (xi - u*)^2)
  summarise_at(c("wxma", "CL.x", "sqConf"), sum) %>%
  mutate(SumConfsq = CL.x^2) %>%
  mutate(Neff = SumConfsq / sqConf)

## Step B of SD calculation and exponentiation of results
SumAllDataUseQ3 <-
  inner_join(SDSumQ3log, SumAllDataQ32, by = c("Species")) %>%
  mutate(S2 = (Neff/((Neff-1) *CL.x)) * wxma) %>%
  mutate(S = (sqrt(S2/Neff))) %>% 
  mutate(upSD = exp(WtAve + S)) %>%
  mutate(dnSD = exp(WtAve - S)) %>%
  mutate_at("WtAve", expfunc)

## Save results for question
Q32 <- data.frame(SumAllDataUseQ3)
# write.xlsx(Q32, "Survey results by question and species_updated Nov 2020.xlsx",
#           sheetName = "Q3", append = TRUE)


## Add Q3 results to dataframe with other questions
lamValues <- inner_join(SumAllDataUseQ3, Dmax_values, by = "Species")
lamValues <- lamValues %>%
  select(WtAve, Species2)
colnames(lamValues) <- c("AveLambda", "Lname")

Survey2 <- inner_join(Survey, lamValues, by = "Lname")
# saveRDS(Survey2, "Survey results updated Oct 2.RDS")
# write.xlsx(Survey2, "Survey results updated Oct 2.xlsx")

#### Step 4 Qualitative Questions: Calculate weighted average and SD ------------------------------------------------
#Need to develop a way to incoprorate weights into answers
#Idea: Use the person's CL as the count, so that someone with low CL counts as less than 1 response
#This means I need to add confid level column to these dataframes
#I could also have a "No response" category for this question; needs to be weighted

#### Q9: Catchment preference based on size ------------------------------------------------
### Filter responses for Q9 from dataframe df, drop NAs, and summarize
Q9 <- df %>%
  filter(Question == 9) %>%
  drop_na(Value, CL) %>%
  group_by(Species, Value) %>%
  mutate_at("CL", as.numeric) %>%
  summarise_at("CL", sum)
## Set levels of dataframe
Q9$Value <- factor(Q9$Value, levels = c("No preference", "Smaller", "Larger"))

Rank9 <- Q9 %>%
  pivot_wider(names_from = Value, values_from = CL) %>%
  mutate(Small2 = replace_na(Smaller, 0)) %>%
  mutate(Diff = (Larger + Small2) - `No preference`) %>%
  arrange(Diff)
Rank92 <- as.vector(Rank9$Species)
Rank9$Species <- factor(Rank9$Species, levels = Rank92)

## Define function for figure
FuncQ9 <- function(dataQ9){
  ggplot(data = dataQ9, aes(x = Value, fill = Value, y = CL)) +
    geom_col(position = "dodge", color = "black") +
    facet_wrap(~Species) +
    theme_bw() +
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), 
          title = element_text(size = 18), strip.text.x = element_text(size = 16), legend.position = "none") +
    scale_fill_viridis_d(option = "E") +
    ylab("Weighted Response (Sum of confidence levels)") +
    xlab("Size") +
    ggtitle("Size Preference of Catchment")
}
## Create figure
FuncQ9(Q9) 

## Rank species in the figure according to the difference between having a preference (whether large or small) and not having a preference
Q9Rank <- Q9
Tmp9Rank <- Q9Rank %>%
  group_by(Species) %>%
  summarize_at("CL", sum) %>%
  arrange(CL)
Q9RankSpp <- as.vector(Tmp9Rank$Species)
Q9Rank$Species <- factor(Q9Rank$Species, levels = Q9RankSpp)
FuncQ9(Q9Rank) + ggtitle("Size Preference of Catchment Organized by Difference Between Size Choice and No Preference (Low to High)")

#### Q10: Preference for catchment with conspecifics -------------------------------------------------

### Filter only results for Q10, replace NA with "no answer", calculate sum of CL
Q10 <- df %>%
  filter(Question == 10) %>%
  drop_na(CL) %>%
  mutate(Response = replace_na(Value, "No answer")) %>%
  group_by(Species, Response) %>%
  mutate_at("CL", as.numeric) %>%
  summarise_at("CL", sum)

## Set levels
Q10$Response <- factor(Q10$Response, levels = c("No answer", "No", "Yes"))

## Define function for figure
PlotQ10 <- function(data, x, fill, y){
  ggplot(data = data, aes_string(x = x, fill = fill, y = y)) +
    geom_col(position = "dodge", color = "black") +
    facet_wrap(~Species) +
    theme_bw() +
    theme(legend.position = "none", axis.text = element_text(size = 16), 
          axis.title = element_text(size = 18), 
          title = element_text(size = 18), strip.text.x = element_text(size = 16)) +
    scale_fill_viridis_d(option = "E") +
    ylab("Weighted Response (Sum of confidence levels)") +
    xlab("Attraction") +
    ggtitle("Attraction to Catchment with Conspecifics")
}

## Calculate results as % of each category (yes, no, no answer)
Q10RankYes <- Q10 %>%
  group_by(Species) %>%
  mutate(Total = sum(CL)) %>%
  mutate(
    Perc = case_when(
      Response == "No" ~ CL / Total,
      Response == "Yes" ~ CL / Total,
      Response == "No answer" ~ CL / Total))

## Plot % of each category
PlotQ10(Q10RankYes, "Response", "Response", "Perc") + ylab("Weighted Response (% of each category)")


### Other ways to rank results:
Q10Rank <- Q10

## Rank according to confidence level (low to high)
Tmp10Rank <- Q10Rank %>%
  group_by(Species) %>%
  summarize_at("CL", sum) %>%
  arrange(CL)
Q10RankSpp <- as.vector(Tmp10Rank$Species)
Q10Rank$Species <- factor(Q10Rank$Species, levels = Q10RankSpp)

## Create plot
PlotQ10(Q10Rank, "Response", "Response", "CL") +
  ggtitle("Attraction to catchment with conspecifics, arranged by confidence level")

## Rank responses according to difference between "yes" and "no"; uses actual values and not %
Rank10 <- Q10Rank %>%
  pivot_wider(names_from = Response, values_from = CL) %>%
  mutate(Diff = (Yes - No)) %>%
  arrange(Diff)
Rank102 <- as.vector(Rank10$Species)
Q10Rank$Species <- factor(Q10Rank$Species, levels = Rank102)

PlotQ10(Q10Rank, "Response", "Response", "CL") +
  ggtitle("Attraction to Catchment with Conspecifics, Arranged by Difference Between Yes and No (low to high)")




