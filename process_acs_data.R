# Anderson Monken
# Dec 11, 2020
# process ACS data

# WARNING - requires over 64GB of RAM on machine

# packages
library(pacman)
p_load(tidyverse)
p_load(ipumsr)
p_load(questionr)

# read in the data
ddi <- read_ipums_ddi("data/usa_00003.xml")
data <- read_ipums_micro(ddi)

# remove unknown counties from the dataset
data <- data %>% filter(COUNTYFIP != 0)

# make a string fips code that we can use for grouping
data$FIPS <- paste0(str_pad(as.character(data$STATEFIP),2,side = 'left', pad = '0') , 
                    str_pad(as.character(data$COUNTYFIP),3,side = 'left', pad = '0'))



data_processed <- data %>% 
  mutate(FTOTINC = ifelse(HHINCOME == 9999999, NA, HHINCOME), # remove 9999999 missing ---- person
         FTOTINC = ifelse(FTOTINC == 9999999, NA, FTOTINC), # remove 9999999 missing ---- household
         INCINVST = ifelse(INCINVST  == 999999, NA, INCINVST), # remove 999999 missing ---- person
         TRANTIME = ifelse(TRANTIME  == 0, NA, TRANTIME), # remove 0 missing
         RENTGRS = ifelse(OWNERSHPD %in% c(20,22), RENTGRS, NA) # remove rent numbers if person not renting
  ) %>%
  mutate(race_dummy = ifelse(RACE == 1,1,0), # whites = 1, others = 0
         hispanic_dummy = ifelse(HISPAN %in% c(1,2,3,4), 1, 0), # non-hispanic = 0, hispanic = 1
         insurance_dummy = ifelse(HCOVANY == 2, 1, 0), # insurance = 1, no-insurance = 0
         medicare_dummy = ifelse(HINSCARE == 2, 1, 0), # medicare = 1, no-medicare = 0
         veteran_dummy = ifelse(VETSTAT == 2, 1, 0), # veteran = 1, no-veteran = 0
         poverty_dummy = ifelse(POVERTY < 100, 1, 0), # poverty = 1, no-poverty = 0
         poverty200_dummy = ifelse(POVERTY < 200, 1, 0), # poverty = 1, no-poverty = 0
         povertyextreme15_dummy = ifelse(POVERTY < 15, 1, 0), # poverty = 1, no-poverty = 0
         foodstamp_dummy = ifelse(FOODSTMP == 2, 1, 0), # food stamp = 1, else = 0 ---- household
         renters_dummy = ifelse(OWNERSHP == 2, 1, 0), # food stamp = 1, else = 0 ---- household
         
  ) %>%
  group_by(FIPS, YEAR) %>% 
  summarize(
    avg_hhincome = wtd.mean(HHINCOME, weights = HHWT),
    avg_age = wtd.mean(AGE, weights = PERWT),
    avg_white = wtd.mean(race_dummy, weights = PERWT),
    avg_hispanic = wtd.mean(hispanic_dummy, weights = PERWT),
    avg_hlthinsur = wtd.mean(insurance_dummy, weights = PERWT),
    avg_medicare = wtd.mean(medicare_dummy, weights = PERWT),
    avg_income = wtd.mean(FTOTINC, weights = PERWT),
    avg_invincome = wtd.mean(INCINVST, weights = PERWT),
    poverty_rate = wtd.mean(poverty_dummy, weights = PERWT),
    poverty200_rate = wtd.mean(poverty200_dummy, weights = PERWT),
    povertyX_rate = wtd.mean(povertyextreme15_dummy, weights = PERWT),
    vet_rate = wtd.mean(veteran_dummy, weights = PERWT),
    avg_transit = wtd.mean(TRANTIME, weights = PERWT),
    rent_perc = wtd.mean(renters_dummy, weights = HHWT),
    avg_rent = wtd.mean(RENTGRS, weights = HHWT)
  )

saveRDS(data_processed, 'data/acs_fips_annual_data.rds')



                          