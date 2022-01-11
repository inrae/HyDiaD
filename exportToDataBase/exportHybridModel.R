


library(tidyverse)

rm(list = ls())
# conversion between climatic model names
model_FIC <- read_csv("./exportToDataBase/climatic_model.csv") %>% 
  suppressMessages() 

# conversion between species names
species <- read_csv("./exportToDataBase/species.csv") %>% 
  suppressMessages() %>% 
  dplyr::select(species_id, latin_name, Lname = l_name) %>% 
  drop_na()

# create the structure for the output
# data = data.frame(species = character(), climatic_scenario = character(), climatic_model_id = integer(),  climatic_model_code = character(), 
#                   basin_name = character(), basin_id = integer(), year = integer(), 
#                   Nit = double(), HSI = double(), saturation_rate = double(), stringsAsFactors = FALSE)
data = tibble()
for (filename in dir(path = "./data_output", pattern = 'HyDiaDResults_')) {
  
  # load model outputs
  results <- read_rds(paste0("data_output/", filename))
  
  ## Pluck all the  (and only all the) Nit results from the diffrent models
  #Tmp_nit <- modify_depth(results[[1]], 2, pluck("Nit"))
  
  # load useful parameters
  surf_id = results %>% pluck(2) %>%  dplyr::select(Basin_name, Surf, basin_id)
  Dmax = results %>% pluck(1, 1, 'ParmSet', 'Dmax')
  Lname = results %>% pluck(1, 1, 'ParmSet', 'Lname')
  climatic_scenario = results %>% pluck(1, 1, 'ParmSet', 'rcp')
  
  cat(Lname, ' ', climatic_scenario, "\n")
  
  # join model output with  climatic model name (within  the list name)
  models = tibble( long_name = results %>% pluck(1,1) %>% names()) %>%
    filter(long_name != "ParmSet") %>%
    mutate(name = str_remove(long_name, "Ann_Enviro_") ) %>% 
    left_join(model_FIC %>% 
                dplyr:: select(climatic_model_id, climatic_model_code, climatic_model_hm), 
              by = c("name" = "climatic_model_hm")) 
  
  #loop on climatic model in the model output 
  for (i in 1:nrow(models)) {
    cat("  ", models$long_name[i], "\n")
    # load Nit
    nit <- results %>% 
      pluck(1, 1, models$long_name[i], 'Nit') %>%
      as_tibble(rownames = "basin_name") %>% 
      #dplyr::select(basin_name, num_range("", 1951:2100)) %>% 
      pivot_longer(cols = -basin_name, names_to = 'year', values_to = 'Nit') 
    
    # join with HSI
    hsi = results %>% pluck(1, 1, models$long_name[i], 'HSI') %>% 
      as_tibble(rownames = "basin_name") %>% 
      #dplyr::select(basin_name, num_range("", 1951:2100)) %>% 
      pivot_longer(cols = -basin_name, names_to = 'year', values_to = 'HSI')
    
    data <-  bind_rows(data,
                       nit %>% 
                         inner_join(hsi,
                                    by = c("basin_name", "year")) %>%
                         # associate surface area of the basin
                         inner_join(surf_id, by = c("basin_name" = "Basin_name")) %>% 
                         # calulate saturation rate
                         mutate(saturation_rate = Nit / (HSI * Surf * Dmax)) %>%
                         # convert year in integer
                        # mutate(year = as.integer(year)) %>%       
                         # add species and climatic model
                         mutate(Lname = Lname, 
                                climatic_scenario = climatic_scenario,
                                climatic_model_code = models$climatic_model_code[i], 
                                climatic_model_id =  models$climatic_model_id[i] ) 
                        
    )
  }
}


# export
data <- data  %>%
  mutate(phase = case_when(str_detect(year, 'Initial') ~ 'initial',
                           str_detect(year, 'Burn') ~ 'burn', 
                           TRUE ~ 'run'),
         year =  as.integer(year)) %>% suppressWarnings() %>% 
  left_join(species %>% dplyr::select(species_id, latin_name, Lname), by = c("Lname")) %>% 
  dplyr::select(c(species_id, latin_name, climatic_scenario, climatic_model_id, climatic_model_code, 
           basin_id, basin_name, 
           year, phase,
           Nit, HSI, saturation_rate)) 
data %>% 
  write_csv(file = "./exportToDataBase/data_output/hybridModelExport.csv")

data %>% 
  write_rds(file = "./exportToDataBase/data_output/hybridModelExport.rds")




