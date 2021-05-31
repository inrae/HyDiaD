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
## -----------------------------------------
## Packages:  (uncomment as required)

library(tidyverse)
library(tidyselect)
library(rlang)

library(viridisLite) #Need for viridis 
library(viridis) 
library(RColorBrewer) #Used for color scale for annotations

#library(devtools)
#install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)

## ---------------------------
rm(list=ls())

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

####Step 1: Organize results from running code in file "HSDM_functions" --------------------
## Sourced functions and loaded datasets:
# results <- readRDS("Alosa_rcp85_Jan2021.RDS")
results <- readRDS("data_output/results_AAlosa_rcp85.RDS")
#results <- readRDS("Alosa_rcp45_Jan2021.RDS")
#results <- readRDS("Fallax_rcp85_Jan2021.RDS")
#results <- readRDS("Fallax_rcp45_Jan2021.RDS")

Species <- pluck(results, 1, 1, 'ParmSet', 'Lname')
rcp <- pluck(results,1,1,'ParmSet', 'rcp')



## Get basin information into a datafram
## To rank, first list the order wanted for the countries
## Then arrange by both country and latitude
basin_area <- data.frame("Basin" = results[[2]]$Basin_name,
                         "SA" = results[[2]]$Surf, 
                         "Lat" = results[[2]]$Lat, 
                         "Country" = results[[2]]$country,
                         "Long" = results[[2]]$Long) %>% 
  mutate(Country = factor(Country, 
                          levels = rev(c("Morocco", "Portugal", "Spain", "France", 
                                         "Germany", "England", "Wales", "Ireland", "Scotland",
                                         "Denmark", "Sweden", "Norway") ))) %>% 
  arrange(Country, desc(Lat)) 


# ## Then define as a ranked vector for use later
RankCL <- basin_area %>%
  dplyr::select(Basin) %>%
  unlist(use.names = FALSE)

basin_area <- basin_area %>% mutate(Basin = factor(Basin, levels = RankCL))
rm(RankCL)



### This function takes the df for each climate model and calculations saturation rate (SR) and density
## Results is df with SR, HSI, Density, and Nit for each catchment across all years
FUNsubset <- function(results, model, basin_area){
  
  reshapeResult = function(results, model, target) {
    ## remove initial and burn coulums
    ## new column with Basin name
    ## remove 'X' in column name
    ## pivot longer
    return( data.frame(pluck(results,1,1, model, target)) %>% 
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

### Subsets the data for each climate model using FUNsubset:
SS_cs <- FUNsubset(results, 
                   model = 'Ann_Enviro_cs', 
                   basin_area = basin_area)
SS_cn <- FUNsubset(results, 
                   model = 'Ann_Enviro_cn', 
                   basin_area = basin_area)
SS_no <- FUNsubset(results, 
                   model = 'Ann_Enviro_no', 
                   basin_area = basin_area)


#### Step 2: Arrange the df for each climate model and prepare it for use with Complexheatmap -----------------------------
### This creates a figure that includes SR, Density, HSI and average abundance for one species and one rcp
## First organize the data for the heatplots for each climate model:

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

FUNprep2 <- function(df){
  
  reshape_df = function(df, target){
    df %>% 
      dplyr::select(Basin, Country, Lat, Year, target) %>% 
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

## Run the funcion for each climate model
Results_cn <- FUNprep2(df = SS_cn)
Results_cs <- FUNprep2(SS_cs)
Results_no <- FUNprep2(SS_no)


## Next calculate the average abundance from 1950-1980 for use in annotation for each climate model
# This calculates a mean and max, but I only used the mean for the figures
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

## Run the function for each climate model:
Nit_cn <- FUNanno(data = Results_cn)
Nit_cs <- FUNanno(data = Results_cs)
Nit_no <- FUNanno(data = Results_no)

## Last, calculate a mean that combines all three climate models for SR, Den, HSI, and ave abund
SS_ave <- SS_cn %>%
  dplyr::select(Basin, Lat, Country, Year, SR, Den, HSI, Nit) %>% mutate(model = 'cn') %>% 
  bind_rows(SS_cs %>%
              dplyr::select(Basin, Lat, Country, Year, SR, Den, HSI, Nit) %>% mutate(model = 'cs')) %>% 
  bind_rows(SS_no %>%
              dplyr::select(Basin, Lat, Country, Year, SR, Den, HSI, Nit) %>% mutate(model = 'no')) %>% 
  group_by(Basin, Lat, Country, Year) %>% 
  summarise(AveSR = mean(SR), 
            AveDen = mean(Den), 
            AveHSI = mean(HSI), 
            AveNit = mean(Nit), 
            .groups = 'drop')  


## Define a function to create a df for the average for each output type:
FUNprepave <- function(data, target){
  data_use <- data %>% 
    dplyr::select(Basin, Year, target) %>% 
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

## Run the function for Density, HSI, SR:
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


#### Step 3: Create the heatplots ------------------------------
### This has code to make the heatplots separately for each climate model
### and also as an average for all three climate models
#### Heatplot for only climate model cn: ---------------------------------------

singleHeatmap = function(results, target, title, scale, rightAnnotation = NULL) {
  hp <- Heatmap(results[[target]],
          name = title,
          col = viridis(6),
          row_order = rownames(results[[target]]),
          column_order = colnames(results[[target]]),
          cluster_row_slices = FALSE,
          #row_split = c(8, 10, 12, 16, 23, 24, 39, 44, 81, 120, 130),
          row_split = Results_cn$df_col,
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


#### Heatplot for only climate model cn: -----------------------------------------
rm(ht_list_cn)
ht_list_cn <- singleHeatmap(Results_cn, 'SR_use', "Saturation Rate", scale = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  singleHeatmap(Results_cn, 'Den_use', "Density (fish/km2)",  scale = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  singleHeatmap(Results_cn, 'HSI_use', "Habitat Suitability", scale = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                rightAnnotation =   annotation(Nit_cn$MeanNit, label = "Ave Abundance \n(1951-1980)"))

png(filename = paste0(Species, '_cn_', rcp,'.png'),
    width = 8,
    height = 10,
    unit = "in",
    res = 300
)
draw(ht_list_cn, heatmap_legend_side = "bottom")
dev.off()


#### Heatplot for only climate model cs: -----------------------------------------
rm(ht_list_cs)
ht_list_cs <- singleHeatmap(Results_cs, 'SR_use', "Saturation Rate", scale = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  singleHeatmap(Results_cs, 'Den_use', "Density (fish/km2)",  scale = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  singleHeatmap(Results_cs, 'HSI_use', "Habitat Suitability", scale = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                rightAnnotation =   annotation(Nit_cs$MeanNit, label = "Ave Abundance \n(1951-1980)"))

## To save as png; change filename as necessary!
png(filename = paste0(Species, '_cs_', rcp,'.png'),
    width = 8,
    height = 10,
    unit = "in",
    res = 300
)
draw(ht_list_cs, heatmap_legend_side = "bottom")
dev.off()


#### Heatplot for only climate model no: ------------------------------------------------------
rm(ht_list_no)
ht_list_no <- singleHeatmap(Results_no, 'SR_use', "Saturation Rate", scale = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  singleHeatmap(Results_no, 'Den_use', "Density (fish/km2)",  scale = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  singleHeatmap(Results_no, 'HSI_use', "Habitat Suitability", scale = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                rightAnnotation =   annotation(Nit_no$MeanNit, label = "Ave Abundance \n(1951-1980)"))

## To save the file as png; change filename as necessary!
png(filename = paste0(Species, '_no_', rcp,'.png'),
    width = 8,
    height = 10,
    unit = "in",
    res = 300
)
draw(ht_list_no, heatmap_legend_side = "bottom")
dev.off()


#### Heatplot for average of all three climate models: --------------------------
## Check the legend for spawner density first!!
rm(ht_list_all)
ht_list_no <- singleHeatmap(Ave_SR, 'Ave_use', "Saturation Rate", scale = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  singleHeatmap(Ave_Den, 'Ave_use', "Density (fish/km2)",  scale = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  singleHeatmap(Ave_HSI, 'Ave_use', "Habitat Suitability", scale = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                rightAnnotation =   annotation(Mean_Nit$MeanNit, label = "Ave Abundance \n(1951-1980)"))

## To save the file as png; change filename as necessary!
png(filename = paste0(Species, '_all_', rcp,'.png'),
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
