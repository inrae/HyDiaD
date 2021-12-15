library(rfishbase)
library(tidyverse)


rm(list = ls())
# fish <- validate_names(c("Alosa alosa", "Alosa fallax", "Salmo salar", "Lampetra fluviatilis", 
#           "Petromyzon marinus", "Acipenser sturio", "Platichthys flesus", "Chelon ramada",
#           "Osmerus eperlanus",  "Salmo trutta", "Anguilla anguilla"))
# fish <- c("Alosa alosa", "Alosa fallax", "Salmo salar", "Lampetra fluviatilis", 
#                          "Petromyzon marinus", "Acipenser sturio", "Platichthys flesus", "Chelon ramada",
#                          "Osmerus eperlanus",  "Salmo trutta", "Anguilla anguilla")

fish <- tibble(latin_name = c("Alosa alosa", "Alosa fallax",  "Lampetra fluviatilis",  "Petromyzon marinus", "Salmo salar", "Salmo trutta",
                              "Acipenser sturio","Osmerus eperlanus", "Anguilla anguilla", "Chelon ramada", "Platichthys flesus")) %>% 
  mutate(name = factor(latin_name,levels = latin_name))

##To find out which table to query
list_fields("Resilience") #stocks
list_fields("Fecundity") #stocks
list_fields("Maturity") #stocks
list_fields("Recruitment") #refrens
list_fields("Abundance") #refrens and stocks
list_fields("Growth")

dat <- stocks(fish, fields = c("Resilience", "Fecundity", "Maturity", "Abundance"))
dat2 <- stocks(fish, fields = c("Fecundity", "Maturity"))
re <- refrens(fish, fields = c("Recruitment", "Abundance"))
pop <- popgrowth(fish)
sto <- stocks(fish)
fec <- fecundity(fish %>% pull(latin_name))
rep <- reproduction(fish %>% pull(latin_name))
mat <- maturity(fish)




fec %>%
  select(Species, Locality, FecundityMin, FecundityMax, FecundityMean, FecundityType, AddInfos) %>% 
  mutate(FecundityMin = as.numeric(FecundityMin)) %>% 
  mutate(fecundity_middleRange = (FecundityMin + FecundityMax) / 2 ) %>% 
  select(Species, Locality, FecundityMin, FecundityMax, fecundity_middleRange) %>% 
  # filter(Species == 'Chelon ramada') %>% 
  #   print(n=Inf)
  drop_na(fecundity_middleRange) %>%  
  group_by(Species) %>%
  summarise(FecundityMean = round(mean(fecundity_middleRange, na.rm = FALSE)),
            FecundityMedian = round(median(fecundity_middleRange, na.rm = FALSE)),
            n = n()) %>% 
  mutate(Species = factor(Species, levels = fish %>% pull(latin_name))) %>% 
  arrange(Species) %>% 
  print( n = Inf)

write_csv(fec3, "./survey/data_output/FisbaseFecundity.csv")


toto <- fecundity('Anguilla anguilla')

saveRDS(fec2, file = "Fecundity data from Fishbase for each species.RDS")
saveRDS(fec3, file = "Average fecundity using data from Fishbase.RDS")
