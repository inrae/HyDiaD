

library(readxl)
library(tidyverse)

# conversion between climatic model names
model_FIC <- read_csv("exportToDataBase/climatic_model.csv") %>% 
  suppressMessages() 

# conversion between species names
species <- read_csv("exportToDataBase/species.csv") %>% 
  suppressMessages() %>% 
  dplyr::select(species_id, latin_name, l_name) %>% 
  drop_na()

# create the structure for the output
data = data.frame(species = character(), climatic_scenario = character(), climatic_model_id = integer(),  climatic_model_code = character(), 
                  basin_name = character(), basin_id = integer(), year = integer(), 
                  Nit = double(), HSI = double(), saturation_rate = double(), stringsAsFactors = FALSE)

for (filename in dir(path = "./data_output", pattern = 'results_')) {
 
  # load model outputs
  results <- readRDS(paste0("data_output/", filename))
  
  ## Pluck all the  (and only all the) Nit results from the diffrent models
  #Tmp_nit <- modify_depth(results[[1]], 2, pluck("Nit"))
  
  # load useful parameters
  surf_id = results %>% pluck(2) %>%  select(Basin_name, Surf, basin_id)
  Dmax = results %>% pluck(1, 1, 'ParmSet', 'Dmax')
  species_name = results %>% pluck(1, 1, 'ParmSet', 'Lname')
  climatic_scenario = results %>% pluck(1, 1, 'ParmSet', 'rcp')
  
  cat(species_name, ' ', climatic_scenario, "\n")
  
  # join model output with  climatic model name (within  the list name)
  models = tibble( long_name = results %>% pluck(1,1) %>% names()) %>% filter(long_name != "ParmSet") %>%
    mutate(name = str_remove(long_name, "Ann_Enviro_") ) %>% 
    left_join(model_FIC %>% select(climatic_model_id, climatic_model_code, climatic_model_hm), by = c("name" = "climatic_model_hm")) 
  
  #loop on climatic model in the model output 
  for (i in 1:nrow(models)) {
    cat("  ", models$long_name[i], "\n")
    data <-  bind_rows(data,
                       # load Nit
                       results %>% pluck(1, 1, models$long_name[i], 'Nit') %>% as.data.frame() %>% 
                         select(num_range("", 1951:2100)) %>% 
                         rownames_to_column("basin_name") %>% 
                         pivot_longer(cols = -basin_name, names_to = 'year', values_to = 'Nit') %>% 
                         # join with HSI
                         inner_join(results %>% pluck(1, 1, models$long_name[i], 'HSI') %>% as.data.frame() %>% 
                                      select(num_range("", 1951:2100)) %>% 
                                      rownames_to_column("basin_name") %>% 
                                      pivot_longer(cols = -basin_name, names_to = 'year', values_to = 'HSI'), 
                                    by = c('basin_name', 'year')) %>% 
                         
                         # associate surface area of the basin
                         inner_join(surf_id, by = c("basin_name" = "Basin_name")) %>% 
                         # calulate saturation rate
                         mutate(saturation_rate = Nit / (HSI * Surf * Dmax)) %>%
                         # conevert year in integer
                         mutate(year = as.integer(year)) %>%       
                         # add species and climatic model
                         mutate(species = species_name, 
                                climatic_scenario = climatic_scenario,
                                climatic_model_code = models$climatic_model_code[i], 
                                climatic_model_id =  models$climatic_model_id[i] ) %>% 
                         # select
                         dplyr::select(names(data))
    )
  }
}

# export
data  %>%
  left_join(species %>% dplyr::select(species_id, latin_name, l_name), by = c("species" = "l_name")) %>% 
  select(c(species_id, latin_name, climatic_scenario, climatic_model_id, climatic_model_code, basin_id, basin_name, year, Nit, HSI, saturation_rate)) %>% 
  write_csv("hybridModelExport.csv")






