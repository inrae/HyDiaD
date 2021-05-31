## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Dr. Betsy Barber
##
## Date Created: 2021-01-20
##
## Copyright (c) Dr. Betsy Barber, 2021
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
## Notes:
##   
##
## ---------------------------
rm(list=ls())
options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

## Packages:  (uncomment as required)

require(tidyverse)
library(viridisLite) #Need for viridis 
library(viridis) 
library(RColorBrewer) #Used for color scale for annotations

#library(devtools)
#install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)

## ---------------------------

## Sourced functions and loaded datasets:
Alosa85 <- readRDS("Alosa_rcp85_Jan2021.RDS")
#Alosa45 <- readRDS("Alosa_rcp45_Jan2021.RDS")
#Fallax85 <- readRDS("Fallax_rcp85_Jan2021.RDS")
#Fallax45 <- readRDS("Fallax_rcp45_Jan2021.RDS")


## ---------------------------

####Step 1: Organize results from running code in file "HSDM_functions" --------------------
results <- Alosa85
#results <- Alosa45
#results <- Fallax85
#results <- Fallax45
### This subsets data by climate model (cn, cs, and no)
## Pluck only the Nit results
Tmp_nit <- modify_depth(results[[1]], 2, pluck("Nit"))

## Separate results for Nit for three climate models:
Tmp_nit_cn <- data.frame(map(Tmp_nit, pluck, "Ann_Enviro_cn"))
Tmp_nit_cs <- data.frame(map(Tmp_nit, pluck, "Ann_Enviro_cs"))
Tmp_nit_no <- data.frame(map(Tmp_nit, pluck, "Ann_Enviro_no"))

## Separate results for hsi for three climate models:
hsicn_df <- data.frame(results[[1]][[1]]$Ann_Enviro_cn$HSI)
hsics_df <- data.frame(results[[1]][[1]]$Ann_Enviro_cs$HSI)
hsino_df <- data.frame(results[[1]][[1]]$Ann_Enviro_no$HSI)

## Get basin information into a dataframe:
basin_area <- data.frame("Basin" = results[[2]]$Basin_name,
                         "SA" = results[[2]]$Surf, 
                         "Lat" = results[[2]]$Lat, 
                         "Country" = results[[2]]$country,
                         "Long" = results[[2]]$Long)
## To rank, first arrange by latitude:
basin_area2 <- basin_area %>%
  arrange(Lat)
## Then list the order wanted for the countries:
basin_area2$Country <- factor(basin_area2$Country, 
                              levels = c("Morocco", "Portugal", "Spain", "France", 
                                         "Germany", "England", "Wales", "Ireland", "Scotland",
                                         "Denmark", "Sweden", "Norway"))
## Then arrange by both country and latitude
basin_area_test <- basin_area2 %>%
  arrange(Country, Lat)
## Then define as a vector for use later
RankCL <- as.vector(basin_area_test$Basin)

## Define column names for dataframes:
colList <- as.character(c(1951:2100))
burnList <- sapply(1:50, function(x){paste("Burn", x, sep = "")})
InitList <- sapply(1:6, function(x){paste("Initial", x, sep = "")})

### This function takes the df for each climate model and calculations SR and density
## Results is df with SR, HSI, Density, and Nit for each catchment across all years
FUNsubset <- function(cliModel, hsiModel, Dmax){
  
  ## Remove burn-in and initial columns, and switch rownames to column
  Tmp_nit_cn2 <- cliModel %>%
    #select(num_range("X", 1951:2100)) %>%
    rownames_to_column("Basin")
  #Rename columns 
  colnames(Tmp_nit_cn2) <- c("Basin", InitList, burnList, colList)
  
  ## join the abundance information and the basin information
  Tmp_nit_cn2 <- inner_join(Tmp_nit_cn2, basin_area, by = "Basin")
  ## rank according to country, then latitude within country
  Tmp_nit_cn2$Basin <- factor(Tmp_nit_cn2$Basin, levels = RankCL)
  
  ## Put in long format for heat plot
  Tmp_nit_cn3 <- Tmp_nit_cn2 %>%
    gather("Year", "Nit", 2:207)
  
  ## Now do the same for the hsi dataframe:
  ## Remove burn-in and initial columns:
  hsicn_df2 <- hsiModel %>%
    #select(num_range("X", 1951:2100)) %>%
    rownames_to_column("Basin")
  ## Rename columns
  colnames(hsicn_df2) <- c("Basin", InitList, burnList, colList)
  ## Put in long format:
  hsicn_df2 <- hsicn_df2 %>%
    gather("Year", "HSI", 2:207)
  
  ## Join nit and hsi df together
  SR <- inner_join(Tmp_nit_cn3, hsicn_df2, by = c("Basin", "Year"))
  ## Make sure it's arranged properly
  SR$Basin <- factor(SR$Basin, levels = RankCL)
  ## Calculate SR and density
  SR_Totals <- SR %>%
    mutate(SR = Nit / (HSI * Dmax * SA)) %>%
    mutate(Den = Nit / SA) %>%
    mutate_at(c("Basin", "Year"), as.factor) %>%
    mutate(SRT = Nit / (Dmax * SA))
  return(SR_Totals)
}

### Subsets the data for each climate model using FUNsubset:
SS_cs <- FUNsubset(cliModel = Tmp_nit_cs, 
                   hsiModel = hsics_df, 
                   Dmax = results[[1]][[1]]$ParmSet$Dmax)
SS_cn <- FUNsubset(cliModel = Tmp_nit_cn,
                   hsiModel = hsicn_df, 
                   Dmax = results[[1]][[1]]$ParmSet$Dmax)
SS_no <- FUNsubset(cliModel = Tmp_nit_no,
                   hsiModel = hsino_df,
                   Dmax = results[[1]][[1]]$ParmSet$Dmax)



#### Step 2: Arrange the df for each climate model and prepare it for use with Complexheatmap -----------------------------
### This creates a figure that includes SR, Density, HSI and average abundance for one species and one rcp
## First organize the data for the heatplots for each climate model:
FUNprep <- function(df){
  ## Only select basin, year, and variable, and change to wide format
  SR <- df %>%
    mutate(as.numeric(Year)) %>%
    dplyr::select(Basin, Country, Lat, Year, SR) %>%
    pivot_wider(names_from = Year, values_from = SR)
  
  Den <- df %>%
    mutate(as.numeric(Year)) %>%
    dplyr::select(Basin, Country, Lat, Year, Den) %>%
    pivot_wider(names_from = Year, values_from = Den)
  
  HSI <- df %>%
    mutate(as.numeric(Year)) %>%
    dplyr::select(Basin, Country, Lat, Year, HSI) %>%
    pivot_wider(names_from = Year, values_from = HSI)
  
  Nit <- df %>%
    mutate(as.numeric(Year)) %>%
    dplyr::select(Basin, Country, Lat, Year, Nit) %>%
    pivot_wider(names_from = Year, values_from = Nit)
  
  cntarr <- arrange(SR, Lat)
  test <- arrange(Den, Lat)
  
  ## Then list the order wanted for the countries:
  if (all(cntarr$Basin == test$Basin)){
    cntarr$Country <- factor(cntarr$Country, 
                             levels = c("Norway", "Sweden", "Denmark", "Scotland",
                                        "Ireland", "Wales", "England", "Germany", "France",
                                        "Spain", "Portugal", "Morocco"))
  } else {print('Warning! Basins not same for SR and density')}
  
  #levels = c("Morocco", "Portugal", "Spain", "France", 
  #          "Germany", "England", "Wales", "Ireland", "Scotland",
  #         "Denmark", "Sweden", "Norway"))
  
  ## Then arrange by both country and latitude
  arr_test <- cntarr %>%
    arrange(Country, Lat)
  ## Then define as a vector for use later
  RankCL <- as.vector(arr_test$Basin)
  RankCnty <- as.vector(cntarr$Country)
  SR$Basin <- factor(SR$Basin, levels = RankCL)
  Den$Basin <- factor(Den$Basin, levels = RankCL)
  HSI$Basin <- factor(HSI$Basin, levels = RankCL)
  Nit$Basin <- factor(Nit$Basin, levels = RankCL)
  
  SR_cnty <- data.frame(SR)
  SR_cnty$Country <- factor(SR$Country, levels = c("Norway", "Sweden", "Denmark", "Scotland",
                                                   "Ireland", "Wales", "England", "Germany", "France",
                                                   "Spain", "Portugal", "Morocco"))
  Den_cnty <- data.frame(Den)
  Den_cnty$Country <- factor(Den$Country, levels = c("Norway", "Sweden", "Denmark", "Scotland",
                                                     "Ireland", "Wales", "England", "Germany", "France",
                                                     "Spain", "Portugal", "Morocco"))
  HSI_cnty <- data.frame(HSI)
  HSI_cnty$Country <- factor(HSI$Country, levels = c("Norway", "Sweden", "Denmark", "Scotland",
                                                     "Ireland", "Wales", "England", "Germany", "France",
                                                     "Spain", "Portugal", "Morocco"))
  Nit_cnty <- data.frame(Nit)
  Nit_cnty$Country <- factor(Nit$Country, levels = c("Norway", "Sweden", "Denmark", "Scotland",
                                                     "Ireland", "Wales", "England", "Germany", "France",
                                                     "Spain", "Portugal", "Morocco"))
  
  df_SR_2 <- SR_cnty %>%
    arrange((Country), desc(Basin)) %>%
    column_to_rownames(var = "Basin")
  df_Den_2 <- Den_cnty %>%
    arrange((Country), desc(Basin)) %>%
    column_to_rownames(var = "Basin")
  df_HSI_2 <- HSI_cnty %>%
    arrange((Country), desc(Basin)) %>%
    column_to_rownames(var = "Basin")
  df_Nit_2 <- Nit_cnty %>%
    arrange((Country), desc(Basin)) %>%
    column_to_rownames(var = "Basin")
  
  
  firstcol = which(colnames(df_SR_2) == "X1951")
  lastcol = which(colnames(df_SR_2) == "X2100")
  df_SR_use = as.matrix(df_SR_2[c(firstcol:lastcol)])
  df_Den_use = as.matrix(df_Den_2[c(firstcol:lastcol)])
  df_HSI_use = as.matrix(df_HSI_2[c(firstcol:lastcol)])
  df_Nit_use = as.matrix(df_Nit_2[c(firstcol:lastcol)])
  
  df_col <- data.frame(Country = df_SR_2$Country)
  rownames(df_col) <- rownames(df_SR_2)
  
  mat_colors <- list(Country = brewer.pal(12, "Paired"))
  names(mat_colors$Country) <- unique(df_SR_2$Country)
  
  labList <- c("X1951", "X1960", "X1970", "X1980", "X1990",
               "X2000", "X2010", "X2020", "X2030", "X2040",
               "X2050", "X2060", "X2070", "X2080", "X2090", "X2100")
  labs_use <- colnames(df_SR_use)
  labs_use[!labs_use %in% labList] <- ""
  
  return(list(SR_use = df_SR_use, Den_use = df_Den_use, 
              HSI_use = df_HSI_use, Nit_use = df_Nit_use,
              df_col = df_col, 
              mat_colors = mat_colors, labs_use = labs_use))
  
}

## Run the funcion for each climate model
Results_cn <- FUNprep(SS_cn)
Results_cs <- FUNprep(SS_cs)
Results_no <- FUNprep(SS_no)

## Next calculate the average abundance from 1950-1980 for use in annotation for each climate model
# This calculates a mean and max, but I only used the mean for the figures
FUNanno <- function(data){
  df_Nit <- data.frame(data$Nit_use[,1:30])
  df_Nit <- rownames_to_column(df_Nit, "Basin")
  Mean_Nit <- df_Nit %>%
    rowwise() %>%
    group_by(Basin) %>%
    summarise(MeanNit = mean(c_across(X1951:X1980)), MaxNit = max(c_across(X1951:X1980)))
  ordr <- as.vector(df_Nit$Basin)
  Mean_Nit$Basin <- factor(Mean_Nit$Basin, levels = ordr)
  Mean_Nit$MeanNit <- as.numeric(Mean_Nit$MeanNit) 
  Mean_Nit$MaxNit <- as.numeric(Mean_Nit$MaxNit)
  return(Mean_Nit)
}

## Run the function for each climate model:
Nit_cn <- FUNanno(data = Results_cn)
Nit_cs <- FUNanno(data = Results_cs)
Nit_no <- FUNanno(data = Results_no)

## Last, calculate a mean that combines all three climate models for SR, Den, HSI, and ave abund
## First combine data from three climate models for each output type:
## Saturation Rate:
tmp_SR_cn <- SS_cn %>%
  dplyr::select(Basin, Lat, Country, Year, SR) %>%
  rename(SR.cn = SR)
tmp_SR_cs <- SS_cs %>%
  dplyr::select(Basin, Lat, Country, Year, SR) %>%
  rename(SR.cs = SR)
tmp_SR_no <- SS_no %>%
  dplyr::select(Basin, Lat, Country, Year, SR) %>%
  rename(SR.no = SR)
tmp2_SR <-data.frame(inner_join(tmp_SR_cn, tmp_SR_cs, by = c("Country", "Basin", "Year", "Lat"))) 
tmp_SR <- data.frame(inner_join(tmp2_SR, tmp_SR_no, by = c("Country", "Basin", "Year", "Lat")))
All_SR <- tmp_SR %>%
  rowwise() %>%
  mutate(AveSR = mean(SR.cn, SR.cs, SR.no)) %>%
  mutate(as.numeric(Year)) %>%
  dplyr::select(Basin, Country, Lat, Year, AveSR) %>%
  pivot_wider(names_from = Year, values_from = AveSR)

## Density
tmp_Den_cn <- SS_cn %>%
  dplyr::select(Basin, Lat, Country, Year, Den) %>%
  rename(Den.cn = Den)
tmp_Den_cs <- SS_cs %>%
  dplyr::select(Basin, Lat, Country, Year, Den) %>%
  rename(Den.cs = Den)
tmp_Den_no <- SS_no %>%
  dplyr::select(Basin, Lat, Country, Year, Den) %>%
  rename(Den.no = Den)
tmp2_Den <-data.frame(inner_join(tmp_Den_cn, tmp_Den_cs, by = c("Country", "Basin", "Year", "Lat"))) 
tmp_Den <- data.frame(inner_join(tmp2_Den, tmp_Den_no, by = c("Country", "Basin", "Year", "Lat")))
All_Den <- tmp_Den %>%
  rowwise() %>%
  mutate(AveDen = mean(Den.cn, Den.cs, Den.no)) %>%
  mutate(as.numeric(Year)) %>%
  dplyr::select(Basin, Country, Lat, Year, AveDen) %>%
  pivot_wider(names_from = Year, values_from = AveDen)

## HSI
tmp_HSI_cn <- SS_cn %>%
  dplyr::select(Basin, Lat, Country, Year, HSI) %>%
  rename(HSI.cn = HSI)
tmp_HSI_cs <- SS_cs %>%
  dplyr::select(Basin, Lat, Country, Year, HSI) %>%
  rename(HSI.cs = HSI)
tmp_HSI_no <- SS_no %>%
  dplyr::select(Basin, Lat, Country, Year, HSI) %>%
  rename(HSI.no = HSI)
tmp2_HSI <-inner_join(tmp_HSI_cn, tmp_HSI_cs, by = c("Country", "Basin", "Year", "Lat")) 
tmp_HSI <- inner_join(tmp2_HSI, tmp_HSI_no, by = c("Country", "Basin", "Year", "Lat"))
All_HSI <- tmp_HSI %>%
  rowwise() %>%
  mutate(AveHSI = mean(HSI.cn, HSI.cs, HSI.no)) %>%
  mutate(as.numeric(Year)) %>%
  dplyr::select(Basin, Country, Lat, Year, AveHSI) %>%
  pivot_wider(names_from = Year, values_from = AveHSI)

## Abundance
tmp_Nit_cn <- SS_cn %>%
  dplyr::select(Basin, Lat, Country, Year, Nit) %>%
  rename(Nit.cn = Nit)
tmp_Nit_cs <- SS_cs %>%
  dplyr::select(Basin, Lat, Country, Year, Nit) %>%
  rename(Nit.cs = Nit)
tmp_Nit_no <- SS_no %>%
  dplyr::select(Basin, Lat, Country, Year, Nit) %>%
  rename(Nit.no = Nit)
tmp2_Nit <-inner_join(tmp_Nit_cn, tmp_Nit_cs, by = c("Country", "Basin", "Year", "Lat")) 
tmp_Nit <- inner_join(tmp2_Nit, tmp_Nit_no, by = c("Country", "Basin", "Year", "Lat"))
All_Nit <- tmp_Nit %>%
  rowwise() %>%
  mutate(AveNit = mean(Nit.cn, Nit.cs, Nit.no)) %>%
  mutate(as.numeric(Year)) %>%
  dplyr::select(Basin, Country, Lat, Year, AveNit) %>%
  pivot_wider(names_from = Year, values_from = AveNit)



## Define a function to create a df for the average for each output type:
FUNprepave <- function(data){
cntarr <- arrange(data, Lat)

## Then list the order wanted for the countries:
cntarr$Country <- factor(cntarr$Country, 
                        levels = c("Norway", "Sweden", "Denmark", "Scotland",
                                    "Ireland", "Wales", "England", "Germany", "France",
                                    "Spain", "Portugal", "Morocco"))

## Then arrange by both country and latitude
arr_test <- cntarr %>%
  arrange(Country, Lat)
## Then define as a vector for use later
RankCL <- as.vector(arr_test$Basin)
RankCnty <- as.vector(cntarr$Country)
data$Basin <- factor(data$Basin, levels = RankCL)

tmp_cnty <- data.frame(data)
tmp_cnty$Country <- factor(data$Country, levels = c("Norway", "Sweden", "Denmark", "Scotland",
                                                 "Ireland", "Wales", "England", "Germany", "France",
                                                 "Spain", "Portugal", "Morocco"))

df_2 <- tmp_cnty %>%
  arrange((Country), desc(Basin)) %>%
  column_to_rownames(var = "Basin")

firstcol = which(colnames(df_2) == "X1951")
lastcol = which(colnames(df_2) == "X2100")
df_use = as.matrix(df_2[c(firstcol:lastcol)])
df_col <- data.frame(Country = df_2$Country)
rownames(df_col) <- rownames(df_2)

mat_colors <- list(Country = brewer.pal(12, "Paired"))
names(mat_colors$Country) <- unique(df_2$Country)

labList <- c("X1951", "X1960", "X1970", "X1980", "X1990",
             "X2000", "X2010", "X2020", "X2030", "X2040",
             "X2050", "X2060", "X2070", "X2080", "X2090", "X2100")
labs_use <- colnames(df_use)
labs_use[!labs_use %in% labList] <- ""

return(list(Ave_use = df_use, 
            df_col = df_col, 
            mat_colors = mat_colors, 
            labs_use = labs_use))

}

## Run the function for Density, HSI, SR:
Ave_SR <- FUNprepave(All_SR)
Ave_Den <- FUNprepave(All_Den)
Ave_HSI <- FUNprepave(All_HSI)

## Calculate average abundance from 1951-1980
df_Nit <- data.frame(Basin = All_Nit$Basin, 
                     Country = All_Nit$Country, 
                     Lat = All_Nit$Lat, 
                     All_Nit[,60:89])
Mean_Nit <- df_Nit %>%
  rowwise() %>%
  group_by(Basin) %>%
  summarise(MeanNit = mean(c_across(X1951:X1980)), MaxNit = max(c_across(X1951:X1980)))
#ordr <- as.vector(df_Nit$Basin)
#Mean_Nit$Basin <- factor(Mean_Nit$Basin, levels = ordr)
Mean_Nit$MeanNit <- as.numeric(Mean_Nit$MeanNit) 
basin_order <- as.vector(rownames(Ave_Den$Ave_use))
Mean_Nit$Basin <- factor(Mean_Nit$Basin, levels = basin_order)
#test2 <- column_to_rownames(Mean_Nit, "Basin")



#Mean_Nit$MaxNit <- as.numeric(Mean_Nit$MaxNit)

#### Step 3: Create the heatplots ------------------------------
### This has code to make the heatplots separately for each climate model
### and also as an average for all three climate models
#### Heatplot for only climate model cn: ---------------------------------------
ht_list_cn = Heatmap(Results_cn$SR_use,
                  name = "Saturation Rate",
                  col = viridis(6),
                  row_order = rownames(Results_cn$SR_use),
                  column_order = colnames(Results_cn$SR_use),
                  cluster_row_slices = FALSE,
                  #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
                  row_split = Results_cn$df_col,
                  row_title_rot = 0,
                  row_title_gp = gpar(fontsize = 8),
                  border = TRUE,
                  column_labels = Results_cn$labs_use,
                  column_title = "Saturation Rate",
                  column_title_gp = gpar(fontsize = 10),
                  row_names_gp = gpar(fontsize = 6),
                  #column_names_centered = TRUE,
                  column_names_gp = gpar(fontsize = 8),
                  #left_annotation = rowAnnotation(df = anno_empty(border = TRUE)),
                  heatmap_legend_param = list(direction = "horizontal",
                                              title_gp = gpar(fontsize = 8),
                                              at = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                              labels_gp = gpar(fontsize = 8))
) +
  Heatmap(Results_cn$Den_use,
          name = "Density (fish/km2)",
          col = viridis(6),
          row_order = rownames(Results_cn$SR_use),
          column_order = colnames(Results_cn$SR_use),
          cluster_row_slices = FALSE,
          #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
          row_split = Results_cn$df_col,
          row_title_rot = 0,
          row_title_gp = gpar(fontsize = 8),
          border = TRUE,
          column_labels = Results_cn$labs_use,
          column_title = "Spawner Density (fish/km2)",
          column_title_gp = gpar(fontsize = 10),
          row_names_gp = gpar(fontsize = 6),
          #column_names_centered = TRUE,
          column_names_gp = gpar(fontsize = 8),
          #left_annotation = rowAnnotation(df = Results_cn$df_col, 
          #                               border = TRUE, show_legend = FALSE,
          #                              col = list(df = col_fun)),
          heatmap_legend_param = list(direction = "horizontal",
                                      title_gp = gpar(fontsize = 8),
                                      at = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
                                      labels_gp = gpar(fontsize = 8))
  ) +
  Heatmap(Results_cn$HSI_use,
          name = "HSI",
          col = viridis(6),
          row_order = rownames(Results_cn$SR_use),
          column_order = colnames(Results_cn$SR_use),
          cluster_row_slices = FALSE,
          #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
          row_split = Results_cn$df_col,
          row_title_rot = 0,
          row_title_gp = gpar(fontsize = 8),
          border = TRUE,
          column_labels = Results_cn$labs_use,
          column_title = "Habitat Suitability",
          column_title_gp = gpar(fontsize = 10),
          row_names_gp = gpar(fontsize = 6),
          #column_names_centered = TRUE,
          column_names_gp = gpar(fontsize = 8),
          #left_annotation = rowAnnotation(df = Results_cn$df_col, 
          #                               border = TRUE, show_legend = FALSE,
          #                              col = list(df = col_fun)),
          #right_annotation = rowAnnotation(Abundance = anno_points(Mean_Nit[2])),
          right_annotation = rowAnnotation(Abundance = anno_points(rev(Nit_cn$MeanNit)), 
                                           width = unit(2, "cm"),
                                           annotation_name_side = "top",
                                           annotation_name_rot = 0,
                                           annotation_name_gp = gpar(fontsize = 10),
                                           annotation_label = "Ave Abundance \n(1951-1980)" #,
                                           #gp = gpar(fontsize = 6)
                                           #axis_param = (
                                           # side = "top" #,
                                           #at = c(0, 0.5, 1), 
                                           #labels = c("zero", "half", "one"),
                                           #labels_rot = 45
                                           #)
          ),
          heatmap_legend_param = list(direction = "horizontal",
                                      title_gp = gpar(fontsize = 8),
                                      at = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                      labels_gp = gpar(fontsize = 8))
  ) 

## To save the file: Change the filename as necessary!
png(filename = "A fallax cn rcp 4.5.png",
    width = 8,
    height = 10,
    unit = "in",
    res = 300
)
draw(ht_list_cn, heatmap_legend_side = "bottom")
dev.off()

#### Heatplot for only climate model cs: -----------------------------------------
ht_list_cs = Heatmap(Results_cs$SR_use,
                  name = "Saturation Rate",
                  col = viridis(6),
                  row_order = rownames(Results_cs$SR_use),
                  column_order = colnames(Results_cs$SR_use),
                  cluster_row_slices = FALSE,
                  #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
                  row_split = Results_cs$df_col,
                  row_title_rot = 0,
                  row_title_gp = gpar(fontsize = 8),
                  border = TRUE,
                  column_labels = Results_cs$labs_use,
                  column_title = "Saturation Rate",
                  column_title_gp = gpar(fontsize = 10),
                  row_names_gp = gpar(fontsize = 6),
                  #column_names_centered = TRUE,
                  column_names_gp = gpar(fontsize = 8),
                  #left_annotation = rowAnnotation(df = anno_empty(border = TRUE)),
                  heatmap_legend_param = list(direction = "horizontal",
                                              title_gp = gpar(fontsize = 8),
                                              at = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                              labels_gp = gpar(fontsize = 8))
) +
  Heatmap(Results_cs$Den_use,
          name = "Density (fish/km2)",
          col = viridis(6),
          row_order = rownames(Results_cs$SR_use),
          column_order = colnames(Results_cs$SR_use),
          cluster_row_slices = FALSE,
          #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
          row_split = Results_cs$df_col,
          row_title_rot = 0,
          row_title_gp = gpar(fontsize = 8),
          border = TRUE,
          column_labels = Results_cs$labs_use,
          column_title = "Spawner Density (fish/km2)",
          column_title_gp = gpar(fontsize = 10),
          row_names_gp = gpar(fontsize = 6),
          #column_names_centered = TRUE,
          column_names_gp = gpar(fontsize = 8),
          #left_annotation = rowAnnotation(df = Results_cn$df_col, 
          #                               border = TRUE, show_legend = FALSE,
          #                              col = list(df = col_fun)),
          heatmap_legend_param = list(direction = "horizontal",
                                      title_gp = gpar(fontsize = 8),
                                      at = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
                                      labels_gp = gpar(fontsize = 8))
  ) +
  Heatmap(Results_cs$HSI_use,
          name = "HSI",
          col = viridis(6),
          row_order = rownames(Results_cs$SR_use),
          column_order = colnames(Results_cs$SR_use),
          cluster_row_slices = FALSE,
          #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
          row_split = Results_cs$df_col,
          row_title_rot = 0,
          row_title_gp = gpar(fontsize = 8),
          border = TRUE,
          column_labels = Results_cs$labs_use,
          column_title = "Habitat Suitability",
          column_title_gp = gpar(fontsize = 10),
          row_names_gp = gpar(fontsize = 6),
          #column_names_centered = TRUE,
          column_names_gp = gpar(fontsize = 8),
          #left_annotation = rowAnnotation(df = Results_cn$df_col, 
          #                               border = TRUE, show_legend = FALSE,
          #                              col = list(df = col_fun)),
          #right_annotation = rowAnnotation(Abundance = anno_points(Mean_Nit[2])),
          right_annotation = rowAnnotation(Abundance = anno_points(rev(Nit_cs$MeanNit)), 
                                           width = unit(2, "cm"),
                                           annotation_name_side = "top",
                                           annotation_name_rot = 0,
                                           annotation_name_gp = gpar(fontsize = 10),
                                           annotation_label = "Ave Abundance \n(1951-1980)" #,
                                           #gp = gpar(fontsize = 6)
                                           #axis_param = (
                                           # side = "top" #,
                                           #at = c(0, 0.5, 1), 
                                           #labels = c("zero", "half", "one"),
                                           #labels_rot = 45
                                           #)
          ),
          heatmap_legend_param = list(direction = "horizontal",
                                      title_gp = gpar(fontsize = 8),
                                      at = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                      labels_gp = gpar(fontsize = 8))
  ) 

## To save as png; change filename as necessary!
png(filename = "A fallax cs rcp 4.5.png",
    width = 8,
    height = 10,
    unit = "in",
    res = 300
)
draw(ht_list_cs, heatmap_legend_side = "bottom")
dev.off()
#### Heatplot for only climate model no: ------------------------------------------------------
ht_list_no = Heatmap(Results_no$SR_use,
                  name = "Saturation Rate",
                  col = viridis(6),
                  row_order = rownames(Results_no$SR_use),
                  column_order = colnames(Results_no$SR_use),
                  cluster_row_slices = FALSE,
                  #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
                  row_split = Results_no$df_col,
                  row_title_rot = 0,
                  row_title_gp = gpar(fontsize = 8),
                  border = TRUE,
                  column_labels = Results_no$labs_use,
                  column_title = "Saturation Rate",
                  column_title_gp = gpar(fontsize = 10),
                  row_names_gp = gpar(fontsize = 6),
                  #column_names_centered = TRUE,
                  column_names_gp = gpar(fontsize = 8),
                  #left_annotation = rowAnnotation(df = anno_empty(border = TRUE)),
                  heatmap_legend_param = list(direction = "horizontal",
                                              title_gp = gpar(fontsize = 8),
                                              at = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                              labels_gp = gpar(fontsize = 8))
) +
  Heatmap(Results_no$Den_use,
          name = "Density (fish/km2)",
          col = viridis(6),
          row_order = rownames(Results_no$SR_use),
          column_order = colnames(Results_no$SR_use),
          cluster_row_slices = FALSE,
          #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
          row_split = Results_no$df_col,
          row_title_rot = 0,
          row_title_gp = gpar(fontsize = 8),
          border = TRUE,
          column_labels = Results_no$labs_use,
          column_title = "Spawner Density (fish/km2)",
          column_title_gp = gpar(fontsize = 10),
          row_names_gp = gpar(fontsize = 6),
          #column_names_centered = TRUE,
          column_names_gp = gpar(fontsize = 8),
          #left_annotation = rowAnnotation(df = Results_no$df_col, 
          #                               border = TRUE, show_legend = FALSE,
          #                              col = list(df = col_fun)),
          heatmap_legend_param = list(direction = "horizontal",
                                      title_gp = gpar(fontsize = 8),
                                      at = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
                                      labels_gp = gpar(fontsize = 8))
  ) +
  Heatmap(Results_no$HSI_use,
          name = "HSI",
          col = viridis(6),
          row_order = rownames(Results_no$SR_use),
          column_order = colnames(Results_no$SR_use),
          cluster_row_slices = FALSE,
          #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
          row_split = Results_no$df_col,
          row_title_rot = 0,
          row_title_gp = gpar(fontsize = 8),
          border = TRUE,
          column_labels = Results_no$labs_use,
          column_title = "Habitat Suitability",
          column_title_gp = gpar(fontsize = 10),
          row_names_gp = gpar(fontsize = 6),
          #column_names_centered = TRUE,
          column_names_gp = gpar(fontsize = 8),
          #left_annotation = rowAnnotation(df = Results_no$df_col, 
          #                               border = TRUE, show_legend = FALSE,
          #                              col = list(df = col_fun)),
          #right_annotation = rowAnnotation(Abundance = anno_points(Mean_Nit[2])),
          right_annotation = rowAnnotation(Abundance = anno_points(rev(Nit_no$MeanNit)), 
                                           width = unit(2, "cm"),
                                           annotation_name_side = "top",
                                           annotation_name_rot = 0,
                                           annotation_name_gp = gpar(fontsize = 10),
                                           annotation_label = "Ave Abundance \n(1951-1980)" #,
                                           #gp = gpar(fontsize = 6)
                                           #axis_param = (
                                           # side = "top" #,
                                           #at = c(0, 0.5, 1), 
                                           #labels = c("zero", "half", "one"),
                                           #labels_rot = 45
                                           #)
          ),
          heatmap_legend_param = list(direction = "horizontal",
                                      title_gp = gpar(fontsize = 8),
                                      at = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                      labels_gp = gpar(fontsize = 8))
  ) 


## To save the file as png; change filename as necessary!
png(filename = "A fallax no rcp 4.5.png",
    width = 8,
    height = 10,
    unit = "in",
    res = 300
)
draw(ht_list_no, heatmap_legend_side = "bottom")
dev.off()

#### Heatplot for average of all three climate models: --------------------------
## Check the legend for spawner density first!!
ht_list_no = Heatmap(Ave_SR$Ave_use,
                     name = "Saturation Rate",
                     col = viridis(6),
                     row_order = rownames(Ave_SR$Ave_use),
                     column_order = colnames(Ave_SR$Ave_use),
                     cluster_row_slices = FALSE,
                     #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
                     row_split = Ave_SR$df_col,
                     row_title_rot = 0,
                     row_title_gp = gpar(fontsize = 8),
                     border = TRUE,
                     column_labels = Ave_SR$labs_use,
                     column_title = "Saturation Rate",
                     column_title_gp = gpar(fontsize = 10),
                     row_names_gp = gpar(fontsize = 6),
                     #column_names_centered = TRUE,
                     column_names_gp = gpar(fontsize = 8),
                     #left_annotation = rowAnnotation(df = anno_empty(border = TRUE)),
                     heatmap_legend_param = list(direction = "horizontal",
                                                 title_gp = gpar(fontsize = 8),
                                                 at = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                                 labels_gp = gpar(fontsize = 8))
) +
  Heatmap(Ave_Den$Ave_use,
          name = "Density (fish/km2)",
          col = viridis(6),
          row_order = rownames(Ave_Den$Ave_use),
          column_order = colnames(Ave_Den$Ave_use),
          cluster_row_slices = FALSE,
          #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
          row_split = Ave_Den$df_col,
          row_title_rot = 0,
          row_title_gp = gpar(fontsize = 8),
          border = TRUE,
          column_labels = Ave_Den$labs_use,
          column_title = "Spawner Density (fish/km2)",
          column_title_gp = gpar(fontsize = 10),
          row_names_gp = gpar(fontsize = 6),
          #column_names_centered = TRUE,
          column_names_gp = gpar(fontsize = 8),
          #left_annotation = rowAnnotation(df = Results_no$df_col, 
          #                               border = TRUE, show_legend = FALSE,
          #                              col = list(df = col_fun)),
          heatmap_legend_param = list(direction = "horizontal",
                                      title_gp = gpar(fontsize = 8),
                                      at = c(0, 1, 2, 3, 4, 5, 6, 7, 8), #Use for A. alosa
                                      #at = c(0, 1, 2, 3, 4), #Use for A. fallax
                                      labels_gp = gpar(fontsize = 8))
  ) +
  Heatmap(Ave_HSI$Ave_use,
          name = "HSI",
          col = viridis(6),
          row_order = rownames(Ave_HSI$Ave_use),
          column_order = colnames(Ave_HSI$Ave_use),
          cluster_row_slices = FALSE,
          #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
          row_split = Ave_HSI$df_col,
          row_title_rot = 0,
          row_title_gp = gpar(fontsize = 8),
          border = TRUE,
          column_labels = Ave_HSI$labs_use,
          column_title = "Habitat Suitability",
          column_title_gp = gpar(fontsize = 10),
          row_names_gp = gpar(fontsize = 6),
          #column_names_centered = TRUE,
          column_names_gp = gpar(fontsize = 8),
          #left_annotation = rowAnnotation(df = Results_no$df_col, 
          #                               border = TRUE, show_legend = FALSE,
          #                              col = list(df = col_fun)),
          #right_annotation = rowAnnotation(Abundance = anno_points(Mean_Nit[2])),
          ## NOTE:Average Abund plots with wrong order of basins, so need to specify that we 
          ## Want the reverse order
          right_annotation = rowAnnotation(Abundance = anno_points(rev(Mean_Nit$MeanNit)),
                                           #row_order = levels(Mean_Nit$Basin),
                                           width = unit(2, "cm"),
                                           annotation_name_side = "top",
                                           annotation_name_rot = 0,
                                           annotation_name_gp = gpar(fontsize = 10),
                                           annotation_label = "Ave Abundance \n(1951-1980)" #,
                                           #gp = gpar(fontsize = 6)
                                           #axis_param = (
                                           # side = "top" #,
                                           #at = c(0, 0.5, 1), 
                                           #labels = c("zero", "half", "one"),
                                           #labels_rot = 45
                                           #)
          ),
          heatmap_legend_param = list(direction = "horizontal",
                                      title_gp = gpar(fontsize = 8),
                                      at = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                      labels_gp = gpar(fontsize = 8))
  ) 


## To save the file as png; change filename as necessary!
png(filename = "A alosa ave 3 clim mod rcp 8.5_v2.png",
    width = 8,
    height = 10,
    unit = "in",
    res = 300
)
draw(ht_list_no, heatmap_legend_side = "bottom")
dev.off()


#### Step 4: Create histograms for HSI and density
## Use All_Den and All_HSI, but can put in long format and then count the number in each bin
## For A. fallax:----------------------------
Density45 <- All_Den %>%
  select(-contains("Burn")) %>%
  select(-contains("Initial")) %>%
  gather(key = "Year", value = "Den", 4:153)

HSI45 <- All_HSI %>%
  select(-contains("Burn")) %>%
  select(-contains("Initial")) %>%
  gather(key = "Year", value = "HSI", 4:153)

Hist_data45 <- inner_join(Density45, HSI45, by = c("Basin", "Country", "Year"))
yrs1 = seq(from = 1980, to = 2010, by = 1)
yrs2 = seq(from = 2070, to = 2100, by = 1)
#yrs = c(yrs1, yrs2)
Hist_data45$Country = factor(Hist_data45$Country, levels = c("Norway", "Sweden", "Denmark", "Scotland",
                                                         "Ireland", "Wales", "England", "Germany", "France",
                                                         "Spain", "Portugal", "Morocco"))

#Hist_data1 <- Hist_data %>%
#  select(-Lat.x, -Lat.y) %>%
#  filter(Year %in% yrs1)
#Hist_data2 <- Hist_data %>%
#  select(-Lat.x, -Lat.y) %>%
#  filter(Year %in% yrs2)

Time_data45 <- Hist_data45 %>%
  select(-Lat.x, -Lat.y) %>%
  filter(Year %in% yrs) %>%
  mutate(Per = case_when(
    Year %in% yrs1 ~ "F",
    Year %in% yrs2 ~ "L"
  )) %>%
  mutate(rcp = "45")

Density85 <- All_Den %>%
  select(-contains("Burn")) %>%
  select(-contains("Initial")) %>%
  gather(key = "Year", value = "Den", 4:153)

HSI85 <- All_HSI %>%
  select(-contains("Burn")) %>%
  select(-contains("Initial")) %>%
  gather(key = "Year", value = "HSI", 4:153)

Hist_data85 <- inner_join(Density85, HSI45, by = c("Basin", "Country", "Year"))
Hist_data85$Country = factor(Hist_data85$Country, levels = c("Norway", "Sweden", "Denmark", "Scotland",
                                                             "Ireland", "Wales", "England", "Germany", "France",
                                                             "Spain", "Portugal", "Morocco"))

Time_data85 <- Hist_data85 %>%
  select(-Lat.x, -Lat.y) %>%
  filter(Year %in% yrs) %>%
  mutate(Per = case_when(
    Year %in% yrs1 ~ "F",
    Year %in% yrs2 ~ "L"
  )) %>%
  mutate(rcp = "85")

All_Time2 <- bind_rows(Time_data45, Time_data85)
All_Time_F <- All_Time2 %>%
  unite(TimeRcp, Per:rcp, sep = "") %>%
  mutate(Spp = "A.fallax")
saveRDS(All_Time_F, file = "Afallax_all_density_plots.RDS") 

## For A. alosa: ---------------------------------------
Density45 <- All_Den %>%
  select(-contains("Burn")) %>%
  select(-contains("Initial")) %>%
  gather(key = "Year", value = "Den", 4:153)

HSI45 <- All_HSI %>%
  select(-contains("Burn")) %>%
  select(-contains("Initial")) %>%
  gather(key = "Year", value = "HSI", 4:153)

Hist_data45 <- inner_join(Density45, HSI45, by = c("Basin", "Country", "Year"))
yrs1 = seq(from = 1980, to = 2010, by = 1)
yrs2 = seq(from = 2070, to = 2100, by = 1)
yrs = c(yrs1, yrs2)
Hist_data45$Country = factor(Hist_data45$Country, levels = c("Norway", "Sweden", "Denmark", "Scotland",
                                                             "Ireland", "Wales", "England", "Germany", "France",
                                                             "Spain", "Portugal", "Morocco"))


Time_data45 <- Hist_data45 %>%
  select(-Lat.x, -Lat.y) %>%
  filter(Year %in% yrs) %>%
  mutate(Per = case_when(
    Year %in% yrs1 ~ "F",
    Year %in% yrs2 ~ "L"
  )) %>%
  mutate(rcp = "45")

Density85 <- All_Den %>%
  select(-contains("Burn")) %>%
  select(-contains("Initial")) %>%
  gather(key = "Year", value = "Den", 4:153)

HSI85 <- All_HSI %>%
  select(-contains("Burn")) %>%
  select(-contains("Initial")) %>%
  gather(key = "Year", value = "HSI", 4:153)

Hist_data85 <- inner_join(Density85, HSI45, by = c("Basin", "Country", "Year"))
Hist_data85$Country = factor(Hist_data85$Country, levels = c("Norway", "Sweden", "Denmark", "Scotland",
                                                             "Ireland", "Wales", "England", "Germany", "France",
                                                             "Spain", "Portugal", "Morocco"))

Time_data85 <- Hist_data85 %>%
  select(-Lat.x, -Lat.y) %>%
  filter(Year %in% yrs) %>%
  mutate(Per = case_when(
    Year %in% yrs1 ~ "F",
    Year %in% yrs2 ~ "L"
  )) %>%
  mutate(rcp = "85")

All_Time2 <- bind_rows(Time_data45, Time_data85)
All_Time_A <- All_Time2 %>%
  unite(TimeRcp, Per:rcp, sep = "") %>%
  mutate(Spp = "A.alosa")
saveRDS(All_Time_A, file = "Alosa_all_density_plots.RDS") 

## Now combine the two dataframes into 1:
All_Time_F <- readRDS("Afallax_all_density_plots.RDS")
All_Time_A <- readRDS("Alosa_all_density_plots.RDS")

All_Time <- bind_rows(All_Time_A, All_Time_F)

ggplot(data = All_Time, aes(x = Den, after_stat(count), fill = TimeRcp, color = TimeRcp)) +
  geom_density(alpha = 0.2, lwd = 1) +
  scale_color_manual(values = viridis(4),
                     name = "Category"#,
                     #labels = c("1980-2010 rcp 4.5", "1980-2010 rcp 8.5", 
                       #         "2070-2100 rcp 4.5", "2070-2100 rcp 8.5")
                     ) +
  scale_fill_manual(values = viridis(4), 
                    name = "Category",
                    labels = c("1980-2010 rcp 4.5", "1980-2010 rcp 8.5", 
                               "2070-2100 rcp 4.5", "2070-2100 rcp 8.5")) +
  theme_classic() +
  xlab("Spawner Density (fish/km2)") + ylab("Count") + ggtitle("Spawner Density")
  
ggplot(data = All_Time, aes(x = HSI, after_stat(count), fill = TimeRcp, color = TimeRcp)) +
  geom_density(alpha = 0.2, lwd = 1) +
  scale_color_manual(values = viridis(4),
                     name = "Category",
                     labels = c("1980-2010 rcp 4.5", "1980-2010 rcp 8.5", 
                                "2070-2100 rcp 4.5", "2070-2100 rcp 8.5")) +
  scale_fill_manual(values = viridis(4), 
                    name = "Category",
                    labels = c("1980-2010 rcp 4.5", "1980-2010 rcp 8.5", 
                               "2070-2100 rcp 4.5", "2070-2100 rcp 8.5")) +
  theme_classic() +
  xlab("HSI") + ylab("Count") + ggtitle("Habitat Suitability")


#### Use these ones:
png(filename = "Shads_density_histogram_Jan21.png",
    width = 9,
    height = 4,
    unit = "in",
    res = 300
)
ggplot(data = All_Time, aes(x = Den, after_stat(count), fill = TimeRcp, color = TimeRcp)) +
  facet_wrap(~Spp, scales = "free_x") +
  geom_density(alpha = 0.2, lwd = 1) +
  scale_color_manual(values = viridis(4),
                     name = "Category",
                     labels = c("1980-2010 rcp 4.5", "1980-2010 rcp 8.5", 
                                "2070-2100 rcp 4.5", "2070-2100 rcp 8.5")) +
  scale_fill_manual(values = viridis(4), 
                    name = "Category",
                    labels = c("1980-2010 rcp 4.5", "1980-2010 rcp 8.5", 
                               "2070-2100 rcp 4.5", "2070-2100 rcp 8.5")) +
  theme_classic() +
  xlab("Spawner Density (fish/km2)") + ylab("Count") + ggtitle("Spawner Density")
dev.off()

png(filename = "Shads_HSI_histogram_Jan21.png",
    width = 10,
    height = 4,
    unit = "in",
    res = 300
)
ggplot(data = All_Time, aes(x = HSI, after_stat(count), fill = TimeRcp, color = TimeRcp)) +
  facet_wrap(~Spp) +
  geom_density(alpha = 0.2, lwd = 1) +
  scale_color_manual(values = viridis(4),
                     name = "Category",
                     labels = c("1980-2010 rcp 4.5", "1980-2010 rcp 8.5", 
                                "2070-2100 rcp 4.5", "2070-2100 rcp 8.5")) +
  scale_fill_manual(values = viridis(4), 
                    name = "Category",
                    labels = c("1980-2010 rcp 4.5", "1980-2010 rcp 8.5", 
                               "2070-2100 rcp 4.5", "2070-2100 rcp 8.5")) +
  theme_classic() +
  xlab("HSI") + ylab("Count") + ggtitle("Habitat Suitability")
dev.off()


ggplot(data = All_Time_A, aes(x = HSI, after_stat(count), fill = TimeRcp, color = TimeRcp)) +
  facet_wrap(~Spp) +
  geom_density(alpha = 0.2, lwd = 1) #+
  scale_color_manual(values = viridis(4),
                     name = "Category",
                     labels = c("1980-2010 rcp 4.5", "1980-2010 rcp 8.5", 
                                "2070-2100 rcp 4.5", "2070-2100 rcp 8.5")) +
  scale_fill_manual(values = viridis(4), 
                    name = "Category",
                    labels = c("1980-2010 rcp 4.5", "1980-2010 rcp 8.5", 
                               "2070-2100 rcp 4.5", "2070-2100 rcp 8.5")) +
  theme_classic() +
  xlab("HSI") + ylab("Count") + ggtitle("Habitat Suitability")




#ggplot(data = Hist_data2, aes(x = Den)) +
 # facet_wrap(~Country) +
#  geom_histogram(binwidth = 0.5)

r45 <- All_Time_F %>%
  filter(TimeRcp == "L45")
r85 <- All_Time_F %>%
  filter(TimeRcp == "L85")
  
ggplot(data = r85, aes(x= HSI)) +
 geom_histogram(binwidth = 0.1)


#  geom_histogram(data = Hist_data2, aes(x = HSI), binwidth = 0.1)

df1 <- Alosa45[[1]][[1]]$Ann_Enviro_cn$HSI
df2 <- Alosa85[[1]][[1]]$Ann_Enviro_cn$HSI
test <- setdiff(df1, df2)
