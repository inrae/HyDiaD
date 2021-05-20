library(tidyverse)

#Define the list of results:
results <- RESULTS_Lampetra

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

## For each climate model:
FUNsubset <- function(cliModel, hsiModel, Dmax){

  ## Remove burn-in and initial columns, and switch rownames to column
  Tmp_nit_cn2 <- cliModel %>%
    select(num_range("X", 1951:2100)) %>%
    rownames_to_column("Basin")
    #Rename columns 
  colnames(Tmp_nit_cn2) <- c("Basin", colList)

  ## join the abundance information and the basin information
  Tmp_nit_cn2 <- inner_join(Tmp_nit_cn2, basin_area, by = "Basin")
  ## rank according to country, then latitude within country
  Tmp_nit_cn2$Basin <- factor(Tmp_nit_cn2$Basin, levels = RankCL)

  ## Put in long format for heat plot
  Tmp_nit_cn3 <- Tmp_nit_cn2 %>%
    gather("Year", "Nit", 2:151)

  ## Now do the same for the hsi dataframe:
  ## Remove burn-in and initial columns:
  hsicn_df2 <- hsiModel %>%
    select(num_range("X", 1951:2100)) %>%
    rownames_to_column("Basin")
  ## Rename columns
  colnames(hsicn_df2) <- c("Basin", colList)
  ## Put in long format:
  hsicn_df2 <- hsicn_df2 %>%
    gather("Year", "HSI", 2:151)
 
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

Lampetra_cs <- FUNsubset(cliModel = Tmp_nit_cs, 
                         hsiModel = hsics_df, 
                         Dmax = results[[1]][[1]]$ParmSet$Dmax)
Lampetra_cn <- FUNsubset(cliModel = Tmp_nit_cn,
                         hsiModel = hsicn_df, 
                         Dmax = results[[1]][[1]]$ParmSet$Dmax)
Lampetra_no <- FUNsubset(cliModel = Tmp_nit_no,
                         hsiModel = hsino_df,
                         Dmax = results[[1]][[1]]$ParmSet$Dmax)

#### The following is to make a heatplot: -------------------------------
library(viridisLite) #Need for viridis 
library(viridis) # colour blind friendly palette, works in B&W also#library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(plot.matrix) #Need for heat plots

## Define a function for the heatplot
FUNheatplot <- function(data, fillvar, scalelabel){

ggplot(data,aes(Year,Basin,fill=fillvar))+
  geom_tile(color= "white",size=0.05) + 
  scale_fill_viridis(name=scalelabel,option ="C") + #,
  #breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  #facet_grid(~Period) +
  scale_y_discrete(breaks = unique(data$Basin)) +
  scale_x_discrete(breaks =c(1951, 1975, 2000, 2025, 2050, 2075, 2100)) +
  theme_minimal(base_size = 8) +
  #labs(title= "A. fallax Model cn, Fish density per km2", x="", y="") + #Add label when call
  theme(
    plot.title=element_text(size = 14, hjust = 0),
    axis.text.y=element_text(size = 10),
    #axis.text.y = element_blank(),
    #strip.background = element_rect(colour="white"),
    #axis.ticks=element_blank(),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size=14),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    legend.title=element_text(size=14),
    legend.key.size = unit(1, "cm"),
    legend.text=element_text(size=12))+
  #geom_hline(aes(yintercept = 4.5), lwd = 1) +
  #geom_hline(aes(yintercept = 12.5), lwd = 1) +
  #geom_hline(aes(yintercept = 16.5), lwd = 1) +
  #geom_hline(aes(yintercept = 46.5), lwd = 1) +
  #geom_hline(aes(yintercept = 50.5), lwd = 1) +
  #geom_hline(aes(yintercept = 76.5), lwd = 1) +
  #geom_hline(aes(yintercept = 77.5), lwd = 1) +
  #geom_hline(aes(yintercept = 84.5), lwd = 1) +
  #geom_hline(aes(yintercept = 74.5), lwd = 1) +
  #geom_hline(aes(yintercept = 90.5), lwd = 1) +
  #geom_hline(aes(yintercept = 77.5), lwd = 1) +
#theme(legend.title=element_text(size=12))+
#theme(legend.text=element_text(size=6))+
removeGrid()#ggExtra

}

## Create heatplot for three climate models for SR and density:
## SR:
hp_Lampetra_cn <- FUNheatplot(data = Lampetra_cn,fillvar = Lampetra_cn$SR,
                           scalelabel = "Saturation Rate")
hp_Lampetra_cs <- FUNheatplot(data = Lampetra_cs, fillvar = Lampetra_cs$SR,
                              scalelabel = "Saturation Rate")
hp_Lampetra_no <- FUNheatplot(data = Lampetra_no, fillvar = Lampetra_no$SR,
                              scalelabel = "Saturation Rate")
## Density:
hp_Lampetra_cn_den <- FUNheatplot(data = Lampetra_cn, fillvar = Lampetra_cn$Den,
                                  scalelabel = "Density")
hp_Lampetra_cs_den <- FUNheatplot(data = Lampetra_cs, fillvar = Lampetra_cs$Den,
                                  scalelabel = "Density")
hp_Lampetra_no_den <- FUNheatplot(data = Lampetra_no, fillvar = Lampetra_no$Den,
                                  scalelabel = "Density")


hp_Lampetra_cn
hp_Lampetra_cn_den
hp_Lampetra_cs
hp_Lampetra_cs_den
hp_Lampetra_no
hp_Lampetra_no_den

#ggsave("Heat plot Alosa fish density updated Oct.tiff", width = 8,
 #      height = 5, units = "in")

## If you want to look at variable by country:
ggplot(data = Lampetra_cn) +
  geom_line(aes(x = Year, y = Nit, group = Basin)) +
  facet_wrap(~Country)



