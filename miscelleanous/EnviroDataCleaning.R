##20/02/2020
#Betsy Barber

#Run scenarios for brt with actual enviro data from FIC and updated abundance information for basins in eurodiad4
##so can choose ideal combination of factors, see if some can be dropped, and decide on ideal lr, tc, # trees, etc to use in dispersal function and projections

# Alosa alosa # -----------------------
#Retrieve updated abundance database, create new basin database with all enviro factors (from diadES and FIC)

#Packages:
require(RPostgres)
library(DBI)
library(glue)
library(tidyverse)

# Step 1: Connect to eurodiad database:

##Connect to db
con <- dbConnect(RPostgres::Postgres(), dbname = 'eurodiad',
                 host = 'citerne.bordeaux.irstea.priv',
                 port = 5432, 
                 user = 'betsy.barber',
                 password = rstudioapi::askForPassword("Database password")
)


##Create query to retrieve Alosa abundance from 1851-1950 when there is a record for presence/absence:
basins_sql_Alosa <- glue_sql("select basin_id, basin_name, latin_name, presence_absence, year_from, year_to, abundance_migratory  
                        from eurodiad4.v_abundance where year_from = 1851 
                        and latin_name = 'Alosa alosa'
                        and presence_absence is not null",
                             .con = con
)

#Send query and retrieve data: -- gives me 253 basins of data
basinsAlosa <- dbSendQuery(con, basins_sql_Alosa) 
sqlAlosa <- dbFetch(basinsAlosa)

#Rename columns for east of use below:
colnames(sqlAlosa) <- c("basin_id", "Basin", "Species", "presence_absence", "year_from", "year_to", "abundance_cat")

#write.csv(sqlAlosa, "Alosa 1800s PA.csv")

##Extract ALL basin information for use in the brt:
## I will then select only basins where Alosa is present in later step
##(should I select for instances where SA, length, etc are not null?)
basins_sql_pv <- glue_sql("SELECT basin_id, basin_name, ecoregion_code, surface_area_drainage_basin, length_main_watercourse, altitude_source FROM eurodiad4.basin",
                          .con = con
)

#Send query and retrieve data -- gives me 350 total basins with data
basinspv <- dbSendQuery(con, basins_sql_pv)
PV <- dbFetch(basinspv)
colnames(PV) <- c("basin_id", "Basin", "ecoregion_name", "Surf", "Length", "Alt")


##Extract environmental information from enviro schema: 
##specify only want records where data is available
FIC_sql <- glue_sql("select * from enviro.eurodiad_enviro
                    where air_temperature is not null
                    and precipitation is not null
                    and sea_surface_temperature is not null
                    and salinity is not null
                    and mixing_layer_depth is not null",
                    .con = con
)

#Send query and retrieve data
enviro_data <- dbSendQuery(con, FIC_sql) 
sqlEnviro <- dbFetch(enviro_data)

##This gives me an average monthly value for 12 months for 110 years

#Next find annual, summer, and winter average values for enviro factors
## Summer = June, July, August
## Winter = December, January, February
## definition from Lassalle et al. 2010

## Annual mean values for all factors by basin:
ann_enviro <- sqlEnviro %>%
  group_by(year, basin_id) %>%
  summarise_at(vars(air_temperature:mixing_layer_depth), mean, na.rm = TRUE)
#rename columns for merging later
colnames(ann_enviro) <- c("year", "basin_id", "ann_air_temp", "ann_precip", "ann_SST", "ann_salinity", "ann_mix_depth")

# Summer mean values for all factors by basin:
summer_enviro <- sqlEnviro %>%
  filter(month %in% c(6, 7, 8)) %>%
  group_by(year, basin_id) %>%
  summarise_at(vars(air_temperature:mixing_layer_depth), mean, na.rm = TRUE)
#rename columns for merging later
colnames(summer_enviro) <- c("year", "basin_id", "summ_air_temp", "summ_precip", "summ_SST", "summ_salinity", "summ_mix_depth")

# Winter mean values for all factors by basin:
winter_enviro <- sqlEnviro %>%
  filter(month %in% c(1, 2, 12)) %>%
  group_by(year, basin_id) %>%
  summarise_at(vars(air_temperature:mixing_layer_depth), mean, na.rm = TRUE)
#rename columns for merging later:
colnames(winter_enviro) <- c("year", "basin_id", "wint_air_temp", "wint_precip", "wint_SST", "wint_salinity", "wint_mix_depth")

####PROBLEM: I've noticed that the number of rows in the summer mean dataframe (20570) is not the same as the winter mean dataframe (20411); they should be the same, so what is going on?####
# annual mean dataframe has same # of rows as summer (20570), so something wrong with winter calculation

## ok, so I should query from sqlEnlviro where there are not 12 months for a basin and year combination; n is # of months that have data for each grouping
test <- sqlEnviro %>%
  count(year, basin_id, basin_name, sort = TRUE) %>%
  filter(n != 12) %>%
  group_by(basin_id, basin_name) 
# This suggests that there are 472 year and basin combinations where there are not 12 months of data available

# if we group and nest again, this parses down to 15 basins without 12 months of data avilable for at least one year
NestTest <- test %>%
  group_by(basin_id, basin_name) %>%
  nest()

tmp <- data.frame(basin_id = test[2], year = test[1])  
#List of all rows related to basin and years that have missing data:
test2 <- sqlEnviro %>%
  inner_join(tmp, by = c("basin_id", "year")) %>%
  select(basin_id:month)
#Nest to double check that have all 15 basins:
NestTest2 <- test2 %>%
  group_by(basin_id, basin_name) %>%
  nest()

#save as .csv file for further reference
write.csv(test2, "AllData_BasinsMissingMonths.csv")
write.csv(test, "GroupedData_BasinsMissingMonths.csv") ##again where n is the number of months that have data (so n = 11 means 11 months have data and only one month is missing)

####Now I can see if any of these basins are in the Alosa dataframe, I will also have to remove them from the enviro data and the basin data that I am using####
## Alosa dataframe (now down to 239 basins, so one listed in filter wasn't in Alosa dataframe)
#tmp4 <- data.frame(Basin = unique(NestTest$basin_id))
#tmp5 <- sapply(1:15, function(x){paste0("basin_id != ", tmp4[x,1], "| ")})
#tmp6 <- paste(tmp5, collapse = " ")
#tmp6
#sqlAlosa2 <- sqlAlosa %>%
 # filter(tmp6) ##I'm not sure what to do to make this work for a != statement, though it works for %in%; will have to write out by hand for now:
sqlAlosa2 <- sqlAlosa %>%
  filter(basin_id != 123& basin_id !=193 & basin_id != 181 & basin_id != 51 & basin_id != 69 & basin_id != 328 &
           basin_id != 21& basin_id != 190& basin_id != 191& basin_id != 74& basin_id != 162& basin_id != 326 & basin_id != 323 & basin_id != 324 & basin_id != 196)

## enviro data, remove basins without 12 months from calculated averages (whole basin removed so doesn't affect averages)
PV2 <- PV %>% #Now 335 basins, which makes sense
  filter(basin_id != 123& basin_id !=193 & basin_id != 181 & basin_id != 51 & basin_id != 69 & basin_id != 328 &
           basin_id != 21& basin_id != 190& basin_id != 191& basin_id != 74& basin_id != 162& basin_id != 326 & basin_id != 323 & basin_id != 324 & basin_id != 196)
sqlEnviro2 <- sqlEnviro %>%
  filter(basin_id != 123& basin_id !=193 & basin_id != 181 & basin_id != 51 & basin_id != 69 & basin_id != 328 &
           basin_id != 21& basin_id != 190& basin_id != 191& basin_id != 74& basin_id != 162& basin_id != 326 & basin_id != 323 & basin_id != 324 & basin_id != 196)

#double check new dataframe using filtering from above
test5 <- sqlEnviro2 %>%
  count(year, basin_id, basin_name, sort = TRUE) %>%
  filter(n != 12) ##This has 0 observations of records with less than 12 months


sqlEnviro2 %>%
  filter(basin_id %in% tmp4) #again, no rows with basin_id in list from tmp4 where was less than 12 months of data

## OK, now I can find the averages (or I could have filtered from those done earlier, but whatever)
## Annual mean values for all factors by basin:
ann_enviro <- sqlEnviro2 %>%
  group_by(year, basin_id) %>%
  summarise_at(vars(air_temperature:mixing_layer_depth), mean, na.rm = TRUE)
#rename columns for merging later
colnames(ann_enviro) <- c("year", "basin_id", "ann_air_temp", "ann_precip", "ann_SST", "ann_salinity", "ann_mix_depth")

# Summer mean values for all factors by basin:
summer_enviro <- sqlEnviro2 %>%
  filter(month %in% c(6, 7, 8)) %>%
  group_by(year, basin_id) %>%
  summarise_at(vars(air_temperature:mixing_layer_depth), mean, na.rm = TRUE)
#rename columns for merging later
colnames(summer_enviro) <- c("year", "basin_id", "summ_air_temp", "summ_precip", "summ_SST", "summ_salinity", "summ_mix_depth")

# Winter mean values for all factors by basin:
winter_enviro <- sqlEnviro2 %>%
  filter(month %in% c(1, 2, 12)) %>%
  group_by(year, basin_id) %>%
  summarise_at(vars(air_temperature:mixing_layer_depth), mean, na.rm = TRUE)
#rename columns for merging later:
colnames(winter_enviro) <- c("year", "basin_id", "wint_air_temp", "wint_precip", "wint_SST", "wint_salinity", "wint_mix_depth")

#These all now have the same # of rows (18920)
## OK, now I am ready to move on....





