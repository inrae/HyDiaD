library(RPostgres)

library(DBI)

library(tidyverse)

rm(list =ls())


# path to files
path_input = 'brt/brt_input/'
path_output = 'brt/brt_output/'

# # connection to database
conn <- dbConnect(RPostgres::Postgres(), dbname = 'eurodiad',
                 host = 'citerne.bordeaux.irstea.priv',
                 port = 5432, 
                 user = 'patrick.lambert',
                 password = rstudioapi::askForPassword("Database password")
)

## list of basin with presence/absence data around 1900 ----
#TODO extract surface_area_drainage_basin, altitude_source and length_main_watercourse from CCM and hydroshed
basin <- dbGetQuery(conn, 
           "SELECT
	DISTINCT basin_id,
	basin_name,
	surface_area_drainage_basin, 
	length_main_watercourse, 
	altitude_source
FROM
	eurodiad4.basin b
INNER JOIN eurodiad4.abundance a
		USING (basin_id)
INNER JOIN eurodiad4.species s
		USING(species_id)
WHERE species_id IN (4, 6, 8, 13, 18, 19, 20, 21, 22, 25, 26)
AND period_comment = '1900';") %>% 
  mutate(log_surface = log(surface_area_drainage_basin), 
         slope = altitude_source/(1000 * length_main_watercourse),
         log_slope = log(slope))

## list of basin with associated presence/absence species around 1900 ----
basin_species <- dbGetQuery(conn, 
                            "SELECT
	DISTINCT basin_id,
	basin_name,
	species_id,
	latin_name
FROM
	eurodiad4.basin b
INNER JOIN eurodiad4.abundance a
		USING (basin_id)
INNER JOIN eurodiad4.species s
		USING(species_id)
WHERE species_id IN (4, 6, 8, 13, 18, 19, 20, 21, 22, 25, 26)
AND period_comment = '1900';")

basin_species %>%
  mutate(latin_name = str_replace(latin_name," ", "_")) %>% 
  group_by(basin_id, basin_name) %>% 
  summarise(nb_species = n_distinct(species_id) ) %>% 
  print( n = Inf)
  
basin_species %>%
  mutate(latin_name = str_replace(latin_name," ", "_")) %>%  
  pivot_wider(id_cols = c(basin_id, basin_name), names_from = latin_name, values_from = species_id)

write_rds(basin_species, paste0(path_input, 'basinSpecies.RDS'))

## ================================================================= ##
## list of AA basins in Eurodiad using in species distribution projection ----
eurodiad_AA <- dbGetQuery(conn, "SELECT
basin_id,
basin_name,
country,
ecoregion_name
FROM
eurodiad4.basin
INNER JOIN eurodiad4.ecoregion e
USING (ecoregion_code)
WHERE
ecoregion_code = ANY (ARRAY[20022,
                            20025,
                            20026,
                            20027,
                            20028])
ORDER BY
country, ecoregion_name;")

write_rds(eurodiad_AA, paste0(path_input, 'eurodiadAA.RDS'))
## ============================================================== ##
## extract environmental predictors
## by year and then average between 1900-1910
c("log_surface", "log_slope", "ecoregion_name", "TempAnnual", "TempSummer", "TempWinter", "PrecAnnual", "PrecSummer", "PrecWinter")

obs_predictors <- dbGetQuery(conn,
"SELECT * FROM enviro.eurodiad_enviro ee;")

write_rds(obs_predictors, paste0(path_input, 'obsPredictors.RDS'))

obs_predictors = read_rds(paste0(path_input, 'obsPredictors.RDS'))

obs_predictors %>% group_by(basin_id, basin_name, year) %>%  
  summarize(TempAnnual = mean(sea_surface_temperature),
            PrecAnnual = mean(precipitation), .groups = 'drop')

obs_predictors %>% filter(month %in% c(1,2,3)) %>% 
  group_by(basin_id, basin_name, year) %>%  
  summarize(TempWinter = mean(sea_surface_temperature),
            PrecWinter = mean(precipitation), .groups = 'drop')
## =============================================================== ##
## extract calibration dataset for a species

species = "'Alosa alosa'"

calibrationSet <- dbGetQuery(conn, paste0("
SELECT
basin_id,
basin_name,
presence_absence,
latin_name,
period_comment
FROM
eurodiad4.abundance a
INNER JOIN eurodiad4.basin b
USING (basin_id)
INNER JOIN eurodiad4.species s
USING (species_id)
WHERE
latin_name LIKE ", species, " AND period_comment = '1900'
AND presence_absence IS NOT NULL;"))

write_rds(calibrationSet, paste0(path_input, 'calibrationSet.RDS'))
# TODO joint with environmental predictors

## ==============================================
# check if  environmental predictors are available for the prediction set for each climatic models