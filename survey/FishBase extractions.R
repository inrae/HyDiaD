library(rfishbase)
library(tidyverse)

fish <- validate_names(c("Alosa alsoa", "Alosa fallax", "Salmo salar", "Lampetra fluviatilis", 
          "Petromyzon marinus", "Acipenser sturio", "Platichthys flesus", "Liza ramada",
          "Osmerus eperlanus",  "Salmo trutta", "Anguilla anguilla"))

##To find out which table to query
list_fields("Resilience") #stocks
list_fields("Fecundity") #stocks
list_fields("Maturity") #stocks
list_fields("Recruitment") #refrens
list_fields("Abundance") #refrens and stocks

dat <- stocks(fish, fields = c("Resilience", "Fecundity", "Maturity", "Abundance"))
dat2 <- species(fish, fields = c("Fecundity", "Maturity"))
re <- refrens(fish, fields = c("Recruitment", "Abundance"))
pop <- popgrowth(fish)
sto <- stocks(fish)
fec <- fecundity(fish)

fec2 <- fec %>%
  select(Species, Locality, FecundityMin, FecundityMax, FecundityMean, FecundityType, AddInfos) 

fec3 <- fec2 %>%
  group_by(Species) %>%
  drop_na(FecundityMin, FecundityMax) %>%
  summarise_at(vars(FecundityMin, FecundityMax), mean)

saveRDS(fec2, file = "Fecundity data from Fishbase for each species.RDS")
saveRDS(fec3, file = "Average fecundity using data from Fishbase.RDS")
