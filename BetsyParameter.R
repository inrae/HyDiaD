library(readxl)
library(tidyverse)


rm(list = ls())
HyDiaDParameter <- read_rds('./data_input/HyDiaDParameter.rds') %>% 
  filter(Lname  %in% c('AAlosa','AFallax')) %>% 
  mutate(source = 'HyDiaD')


BestyParameter <-  read_xlsx("./data_input/Species_Parameters.xlsx") %>% 
  inner_join(HyDiaDParameter %>% 
               dplyr::select(latin_name, Lname, name, species_id) %>%
               filter(Lname  %in% c('AAlosa','AFallax')), 
             by = "Lname") %>% 
  mutate (source =  'Betsy',
          DistMax = NaN, 
          Sdisp = Fsurv,
          Mdisp = -log(Fsurv)/MeanDist,
          withAllee = TRUE,
          withNatalStray = TRUE,
          UsePresence =  FALSE,
          rcp = 'rcp85') %>% 
  dplyr:: select(source, 
                 latin_name, Lname, name, species_id,
                 Dmax, lambda = AveLambda, nbCohorts = cohorts,
                 AgeFirstMat = avAge, DistMean = MeanDist, DistMax,
                 alpha = alpha2, beta = beta2,
                 Sdisp, Mdisp, 
                 gamma  = y2,
                 r, 
                 withAllee, withNatalStray, UsePresence,
                 rcp) 

BestyParameter %>%
  bind_rows(HyDiaDParameter) %>%
  dplyr::select(-c(Lname, name, DistMax)) %>%  
  arrange(latin_name)

write_rds(BestyParameter, './data_input/BetsyParameter.rds')

# comparison of outputs
rm(list  = ls())

hydiad = read_rds('./data_output/HyDiaDResults_AAlosa_rcp85.RDS')
betsy = read_rds('./data_output/BetsyResults_AAlosa_rcp85.RDS')
oldBetsy = read_rds('./data_output/old/results_AAlosa_rcp85.RDS')

basin = 'Nivelle'
period = 2000:2010

hydiad[[1]][[1]]$Ann_Enviro_cn$Nit %>% as_tibble(rownames = 'basin') %>% 
  mutate(source = 'hydiad') %>% 
  filter(basin == !!basin) %>% 
  dplyr::select(source, basin,  num_range("",period)) %>% 
bind_rows(betsy[[1]][[1]]$Ann_Enviro_cn$Nit %>% as_tibble(rownames = 'basin') %>% 
  mutate(source = 'betsy') %>% 
  filter(basin == !!basin) %>% 
  dplyr::select(source, basin,  num_range("",period))) %>% 
  bind_rows(
    oldBetsy[[1]][[1]]$Ann_Enviro_cn$Nit %>% as_tibble(rownames = 'basin') %>%
      mutate(source = 'oldBetsy') %>% 
      filter(basin == !!basin) %>% 
      dplyr::select(source, basin,  num_range("",period))
  )


# ======================================================================
#names(hydiad[[1]][[1]]$ParmSet)
#names(oldBetsy[[1]][[1]]$ParmSet)
names_hydiad = c('Lname', 'gamma', 'alpha', 'beta', 'lambda', 'Dmax', 'r', 'eh1', 'eh2', 'Sdisp', 
                 'rcp', 'DistMean', 'withAllee', 'withNatalStray', 'UsePresence',  'avAge', 'bins')
hydiad[[1]][[1]]$ParmSet[names_hydiad] %>% 
  as_tibble() %>%   
  add_column(source = 'hydiad', .before = 1) %>% 
  bind_rows(
    betsy[[1]][[1]]$ParmSet[names_hydiad] %>% 
      as_tibble() %>%   
      mutate(source = 'betsty')) %>% 
  bind_rows(oldBetsy[[1]][[1]]$ParmSet[c('Lname', 'y', 'a', 'b', 'lambda', 'Dmax', 'r', 'eh1', 'eh2', 'FSurv',
                                         'rcp', 'MeanDist', 'allee', 'NatalStray', 'UsePresence',  'avAge', 'bins')] %>% 
              
              as_tibble() %>% 
              rename(gamma = y, alpha = a, 
                     beta= b,
                     Sdisp = FSurv, 
                     DistMean = MeanDist,
                     withAllee = allee,
                     withNatalStray = NatalStray ) %>%    
              mutate(source = 'oldBetsy',
                     withAllee = withAllee == 'yes',
                     withNatalStray = withNatalStray == 'yes', 
                     UsePresence = UsePresence == 'yes'))

