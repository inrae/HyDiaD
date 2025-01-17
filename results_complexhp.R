# ----------------------------------------------------------------------- #
#
# Script name:
#
# Purpose of script:
#
# Author: Dr. Betsy Barber
#
# Date Created: 2021-01-20
#
## Author: Dr. Betsy Barber, 
##  Modified by Patrick Lambert
## Date Created: 2020-09-21
## Date Updated: 2022-02-02
##
## Copyright (c) Betsy Barber, 
##               Patrick Lambert
## Email: patrick.mh.lambert@inrae.fr
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# ----------------------------------------------------------------------- #
#
# Notes:
#
#
# 1. Load Packages -----------------------------------------------------------------------
#  (uncomment as required)

library(tidyverse)
library(tidyselect)
library(rlang)

library(viridisLite) #Need for viridis 
library(viridis) 
library(RColorBrewer) #Used for color scale for annotations

#library(devtools)
#install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)

# 2. prepare  workspace --------------------------

## upload HyDiaDParameter and rcp --------------------------------------------------
# rm(list = ls())
 
rcp = 'rcp85'
 
suffix = "Betsy"
HyDiaDParameter <-  read_rds('./data_input/BetsyParameter.rds')


## option memory -----
# options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
# memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

# ## Then define as a ranked vector for use later
# basin_area <- basin_area %>%
#   mutate(Basin = factor(Basin,
#                         levels = basin_area %>%
#                           pull(Basin)))

# 3. Local functions ====================================================

## Extract SR, HSI, Density, and Nit for each catchment across all years
FUNsubset <- function(results, model, basin_area){
  
  reshapeResult = function(results, model, target) {
    ## remove initial and burn coulums
    ## new column with Basin name
    ## remove 'X' in column name
    ## pivot longer
    return( data.frame(pluck(results,1,1, model, all_of(target))) %>% 
              dplyr::select(!starts_with(c('Burn', 'Initial'))) %>% 
              rownames_to_column('Basin') %>% 
              # mutate(Basin = factor(Basin, levels = RankCL)) %>% 
              pivot_longer(cols = starts_with('X'), 
                           names_prefix = 'X', 
                           names_to = 'Year', 
                           values_to = target) %>% 
              mutate(Year = as.numeric(Year)) )
  }
  
  ## get Dmax
  Dmax = pluck(results, 1, 1, 'ParmSet', 'Dmax')
  
  ## extract Nit and HSI for the given model
  nit <- reshapeResult(results = results, model = model, target = 'Nit' ) 
  hsi <- reshapeResult(results = results, model = model, target = 'HSI' ) 
  
  ## Join nit and hsi df together
  ## arrange properly
  ## Calculate SR and density
  SR <- nit %>%  inner_join(hsi, by = c("Basin", "Year")) %>% 
    mutate(Basin = factor(Basin, levels = levels(basin_area$Basin))) %>% 
    inner_join(basin_area, by = "Basin") %>%  
    mutate(SR = Nit / (HSI * Dmax * SA),
           Den = Nit / SA, 
           SRT = Nit / (Dmax * SA))
  return(SR)
}
# define label and color 
FUNlabel_color = function(data) {
  
  df_col <- data %>% 
    dplyr::distinct(Basin, Country) %>% 
    arrange(Country, Basin) %>% 
    column_to_rownames('Basin') 
  
  mat_colors <-  brewer.pal(12, "Paired")
  names(mat_colors) <-  levels(data$Country) %>% rev()
  
  labs_use <- data %>% dplyr::select(Year) %>% distinct() %>% unlist(use.names = FALSE)
  labs_use[!labs_use %in% c("1951", "1960", "1970", "1980", "1990",
                            "2000", "2010", "2020", "2030", "2040",
                            "2050", "2060", "2070", "2080", "2090", "2100")] <- ""
  
  return(list(df_col = df_col, 
              mat_colors = mat_colors, 
              labs_use = labs_use))
}

### Reshape dataframe ----
FUNprep2 <- function(df){
  
  reshape_df = function(df, target){
    df %>% 
      dplyr::select(Basin, Country, Lat, Year, all_of(target)) %>% 
      pivot_wider(names_from = Year, values_from = target) %>% 
      arrange(Country, Basin) %>%
      column_to_rownames(var = "Basin") %>% 
      dplyr::select(-c('Country', 'Lat')) %>% 
      as.matrix()
  }
  SR_use <- reshape_df(df, target = 'SR')
  Den_use <- reshape_df(df, target = 'Den')
  HSI_use <- reshape_df(df, target = 'HSI')
  Nit_use <- reshape_df(df, target = 'Nit')
  
  label_color = FUNlabel_color(df)
  
  return(list(SR_use = SR_use, Den_use = Den_use, 
              HSI_use = HSI_use, Nit_use = Nit_use,
              df_col = label_color$df_col, 
              mat_colors = label_color$mat_colors, 
              labs_use = label_color$labs_use))
}

### Next calculate the average abundance from 1950-1980 for use in annotation ----
# This calculates a mean and max  for each climate model, 
# but  the mean is only used for the figures
FUNanno <- function(data){
  Mean_Nit <- data.frame(data$Nit_use) %>% 
    rownames_to_column("Basin") %>% 
    mutate(Basin = factor(Basin, levels = Basin)) %>% 
    rowwise() %>%
    group_by(Basin) %>%
    summarise(MeanNit = mean(c_across(X1951:X1980)), 
              MaxNit = max(c_across(X1951:X1980)))
  return(Mean_Nit)
}

## Create a df for the average for each output type ----
FUNprepave <- function(data, target){
  data_use <- data %>% 
    dplyr::select(Basin, Year, all_of(target)) %>% 
    mutate(Year = as.numeric(Year)) %>% 
    pivot_wider( id_cols = Basin, names_from = Year, values_from = target) %>% 
    column_to_rownames('Basin') %>% 
    as.matrix()
  
  label_color = FUNlabel_color(data)
  
  return(list(Ave_use = data_use, 
              df_col = label_color$df_col, 
              mat_colors = label_color$mat_colors, 
              labs_use = label_color$labs_use))
}

## Create the heatplots ------------------------------
### This has code to make the heatplots for one climate model 
### or the average model

singleHeatmap = function(results, target, title, scale, rightAnnotation = NULL) {
  hp <- Heatmap(results[[target]],
                name = title,
                col = viridis(6),
                row_order = rownames(results[[target]]),
                column_order = colnames(results[[target]]),
                cluster_row_slices = FALSE,
                #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
                row_split = results$df_col,
                row_title_rot = 0,
                row_title_gp = gpar(fontsize = 8),
                border = TRUE,
                column_labels = results[['labs_use']],
                column_title = title,
                column_title_gp = gpar(fontsize = 10),
                row_names_gp = gpar(fontsize = 6),
                #column_names_centered = TRUE,
                column_names_gp = gpar(fontsize = 8),
                #left_annotation = rowAnnotation(df = anno_empty(border = TRUE)),
                right_annotation = rightAnnotation,
                heatmap_legend_param = list(direction = "horizontal",
                                            title_gp = gpar(fontsize = 8),
                                            at = scale ,
                                            labels_gp = gpar(fontsize = 8))
  )
}

## prepare annotation for the heatplots ------------------------------
annotation = function(data, label){
  ha <- rowAnnotation(Abundance = anno_points(data), 
                      width = unit(2, "cm"),
                      annotation_name_side = "top",
                      annotation_name_rot = 0,
                      annotation_name_gp = gpar(fontsize = 10),
                      annotation_label = label #,
                      #gp = gpar(fontsize = 6)
                      #axis_param = (
                      # side = "top" #,
                      #at = c(0, 0.5, 1), 
                      #labels = c("zero", "half", "one"),
                      #labels_rot = 45
                      #)
  )
}

# 4. Draw heat plots -----------------------------------------------

## loop over species --------------------------------------------------------------
for (Species in HyDiaDParameter %>% pull(Lname)) {
  
  cat(Species, '\n')
  
  results <- read_rds(file = paste0("data_output/", suffix, "Results_", Species,"_", rcp, ".RDS"))
  
  ## Get basin information into a datafram
  ## To rank, first list the order wanted for the countries
  ## Then arrange by both country and latitude
  basin_area <- tibble("Basin" = results[[2]]$Basin_name,
                       "SA" = results[[2]]$Surf, 
                       "Lat" = results[[2]]$Lat, 
                       "Country" = results[[2]]$country,
                       "Long" = results[[2]]$Long) %>% 
    mutate(Country = factor(Country, 
                            levels = rev(c("Morocco", "Portugal", "Spain", "France", 
                                           "Germany", "England", "Wales", "Ireland", "Scotland",
                                           "Denmark", "Sweden", "Norway") ))) %>% 
    arrange(Country, desc(Lat)) %>% 
    mutate(Basin = factor(Basin,
                          levels = Basin))
  
  
  
  ## loop on climatic model -----------------------------------------
  SS_ave = tibble()
  model_list = c('cn', 'cs', 'no')
  
  for (model in model_list) {
    
    ### Subsets the data for each climate model using FUNsubset: ----
    SS <- FUNsubset(results, 
                    model = paste0('Ann_Enviro_', model), 
                    basin_area = basin_area)
    
    ## Prepare data for one spcies and one climate model ---------------------------------------
    ### This creates a figure that includes SR, Density, HSI and average abundance for one species and one rcp
    ## First organize the data for the heatplots for each climate model:
    Results <- FUNprep2(df = SS)
    
    ## calculate the average abundance from 1950-1980 for each climate model:
    Nit <- FUNanno(data = Results)
    
    ## add the last  results to the SS_ave
    SS_ave <- bind_rows(SS_ave, 
                        SS %>%
                          dplyr::select(Basin, Lat, Country, Year, SR, Den, HSI, Nit) %>% 
                          mutate(model = model))          
    
    ## Draw the heatmaps for one spcies and one climate model -----
    ht_list <- singleHeatmap(results = Results, target = 'SR_use', title = "Saturation Rate", 
                             scale = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
      singleHeatmap(Results, 'Den_use', "Density (fish/km2)",  
                    scale = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
      singleHeatmap(Results, 'HSI_use', "Habitat Suitability", 
                    scale = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                    rightAnnotation = annotation(Nit$MeanNit, 
                                                 label = "Ave Abundance \n(1951-1980)"))
    ## Print the heatmaps for one spcies and one climate model -----
    png(filename = paste0("./data_output/figure/",suffix , "_",Species, '_', model,'_', rcp,'.png'),
        width = 8,
        height = 10,
        unit = "in",
        res = 300
    )
    
    draw(ht_list, heatmap_legend_side = "bottom")
    dev.off()
  }
  
  
  ## Heatplot for average of all three climate models: --------------------------
  ## Check the legend for spawner density first!!
  SS_ave <- SS_ave %>% 
    group_by(Basin, Lat, Country, Year) %>% 
    summarise(AveSR = mean(SR), 
              AveDen = mean(Den), 
              AveHSI = mean(HSI), 
              AveNit = mean(Nit), 
              .groups = 'drop')  
  
  ## Run the function to create a df for the average for Density, HSI, SR:
  Ave_SR <- FUNprepave(SS_ave, 'AveSR')
  Ave_Den <- FUNprepave(SS_ave, 'AveDen')
  Ave_HSI <- FUNprepave(SS_ave, 'AveHSI')
  Ave_Nit <- FUNprepave(SS_ave, 'AveNit')
  
  Results_all = list(Ave_SR = Ave_SR,
                     Ave_Den = Ave_Den, 
                     Ave_HSI = Ave_HSI,
                     Ave_Nit = Ave_Nit)
  
  ## Calculate average abundance from 1951-1980
  Mean_Nit <- data.frame(Ave_Nit$Ave_use) %>% 
    rownames_to_column("Basin") %>% 
    mutate(Basin = factor(Basin, levels = Basin)) %>% 
    pivot_longer(cols = starts_with('X'), 
                 names_prefix = 'X', 
                 names_to = 'Year', 
                 values_to = 'AveNit') %>% 
    mutate(Year = as.numeric(Year)) %>% 
    filter(Year >= 1951 , Year <= 1980) %>% 
    group_by(Basin) %>%
    summarise(MeanNit = mean(AveNit),
              MaxNit = max(AveNit),
              .groups = 'drop')
  
  ht_list_all <- singleHeatmap(Ave_SR, 'Ave_use', "Saturation Rate", 
                               scale = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    singleHeatmap(Ave_Den, 'Ave_use', "Density (fish/km2)",  
                  scale = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
    singleHeatmap(Ave_HSI, 'Ave_use', "Habitat Suitability", 
                  scale = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                  rightAnnotation =   annotation(Mean_Nit$MeanNit, label = "Ave Abundance \n(1951-1980)"))
  
  ## To save the file as png
  png(filename = paste0("./data_output/figure/",suffix , "_",Species, '_all_', rcp,'.png'),
      width = 8,
      height = 10,
      unit = "in",
      res = 300
  )
  draw(ht_list_all, heatmap_legend_side = "bottom")
  dev.off()
}

