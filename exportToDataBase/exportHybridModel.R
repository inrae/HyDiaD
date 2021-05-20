library(dplyr)
library(tidyr)
library(tidyverse)

# conversion between climatic model names
model_FIC <- read.csv("exportToDataBase/climatic_model.csv", stringsAsFactors = FALSE )

# conersion between species names
species <- read.csv("species.csv", stringsAsFactors = FALSE )
species %>% dplyr::select(species_id, latin_name, l_name)

# example of output model
# results <- readRDS("Results_Alosa.RDS")
results <- readRDS("exportToDataBase/Ex_HSDM_Results_wrcp.RDS")

## Pluck only the Nit results
#Tmp_nit <- modify_depth(results[[1]], 2, pluck("Nit"))

# load useful parameters
# TODO need to load climatic scenario when availibel in the RDS file
surf_id = results[[2]] %>%  select(Basin_name, Surf, basin_id)
Dmax = results[[1]][[1]]$ParmSet$Dmax
species_name = results[[1]][[1]]$ParmSet$Lname
climatic_scenario = results[[1]][[1]]$ParmSet$rcp


# join model output with  climatic model name (within  the list name)
models = data.frame(long_name = names(results[[1]][[1]])[-length(results[[1]][[1]])], stringsAsFactors = FALSE) %>%
   mutate(name = substring(long_name, nchar(long_name) - 1)) %>% 
  left_join(model_FIC %>% select(climatic_model_id, climatic_model_code, climatic_model_hm), by = c("name" = "climatic_model_hm")) 

# create the structure for the output
data = data.frame(species = character(), climatic_model_id = integer(),  climatic_model_code = character(), 
                  basin_name = character(), basin_id = integer(), year = integer(), 
                  Nit = double(), HSI = double(), saturation_rate = double(), stringsAsFactors = FALSE)


#loop on climatic model in the model output 

for (i in 1:nrow(models)) {
  cat(models$long_name[i], "\n")
  data <-  bind_rows(data,
                     # load Nit
                     as.data.frame(results[[1]][[1]][[models$long_name[i]]]$Nit) %>% 
                       select(num_range("", 1951:2100)) %>%
                       rownames_to_column("basin_name") %>% 
                       pivot_longer(cols = -1, names_to = 'year', values_to = 'Nit') %>% 
                       # join with HSI
                       inner_join(as.data.frame(results[[1]][[1]][[models$long_name[i]]]$HSI) %>% 
                                    select(num_range("", 1951:2100)) %>%
                                    rownames_to_column("basin_name") %>% 
                                    pivot_longer(cols = -1, names_to = 'year', values_to = 'HSI'), by = c('basin_name', 'year')) %>% 
                       # associate surface area of the basin
                       inner_join(surf_id, by = c("basin_name" = "Basin_name")) %>% 
                       # calulate saturation rate
                       mutate(saturation_rate = Nit / (HSI * Surf * Dmax)) %>%
                       # conevert year in integer
                       mutate(year = as.integer(year)) %>%       
                       # add species and climatic model
                       mutate(species = species_name, climatic_model_code = models$climatic_model_code[i], climatic_model_id =  models$climatic_model_id[i] ) %>% 
                       # select
                       dplyr::select(names(data))
  )
}

# export in CSV

data  %>%
  left_join(species %>% dplyr::select(species_id, latin_name, l_name), by = c("species" = "l_name")) %>% 
  # TODO in the next version this information will be included in the model output 
  mutate(climatic_scenario = climatic_scenario) %>%
  select(c(species_id, latin_name, climatic_scenario, climatic_model_id, climatic_model_code, basin_id, basin_name, year, Nit, HSI, saturation_rate)) %>% 
   write_csv("hybridModelExport.csv")






