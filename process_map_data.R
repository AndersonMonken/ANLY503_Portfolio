# Anderson Monken
# December 9, 2020
# Process map data into RDS files

library(tidyverse)

### House map
df_house <- read_csv('data/2016-precinct-house.csv')
# make a new variable for democrats, republicans, and lump others together
df_house <- df_house %>% 
  mutate(party3 = case_when(party == 'democratic' ~ 'D',
                            party == 'republican' ~ 'R',
                            TRUE ~ 'other'))

df_house_map <- df_house %>% 
  group_by(state, county_name, county_fips, county_lat, county_long, party3) %>% 
  summarize(votes = sum(votes, na.rm = TRUE)) %>% 
  # reshape so each party vote total is its own variable
  pivot_wider(names_from = party3, values_from = votes) %>%
  # create a ratio variables
  mutate(D = if_else(is.na(D), 0, D),
         R = if_else(is.na(R), 0, R),
         other = if_else(is.na(other), 0, other),
         d_ratio = D / (other + R + D),
         r_ratio = R / (other + R + D)) %>% 
  filter(!is.na(d_ratio))

# modify fips code style to match shape file
df_house_map <- df_house_map %>% mutate(FIPS = str_pad(as.character(county_fips),width = 5,side='left',pad='0'))
saveRDS(df_house_map, 'data/house_map.rds')

### Senate map
df_senate <- read_csv('data/2016-precinct-senate.csv')
# make a new variable for democrats, republicans, and lump others together
df_senate <- df_senate %>% 
  mutate(party3 = case_when(party == 'democratic' ~ 'D',
                            party == 'republican' ~ 'R',
                            TRUE ~ 'other'))

df_senate_map <- df_senate %>% 
  group_by(state, county_name, county_fips, county_lat, county_long, party3) %>% 
  summarize(votes = sum(votes, na.rm = TRUE)) %>% 
  # reshape so each party vote total is its own variable
  pivot_wider(names_from = party3, values_from = votes) %>%
  # create a ratio variables
  mutate(D = if_else(is.na(D), 0, D),
         R = if_else(is.na(R), 0, R),
         other = if_else(is.na(other), 0, other),
         d_ratio = D / (other + R + D),
         r_ratio = R / (other + R + D)) %>% 
  filter(!is.na(d_ratio))

# modify fips code style to match shape file
df_senate_map <- df_senate_map %>% mutate(FIPS = str_pad(as.character(county_fips),width = 5,side='left',pad='0'))
saveRDS(df_senate_map, 'data/senate_map.rds')


### Local map
df_local <- read_csv('data/2016-precinct-local.csv')
# make a new variable for democrats, republicans, and lump others together
df_local <- df_local %>% 
  mutate(party3 = case_when(party == 'democratic' ~ 'D',
                            party == 'republican' ~ 'R',
                            TRUE ~ 'other'))

df_local_map <- df_local %>% 
  group_by(state, county_name, county_fips, county_lat, county_long, party3) %>% 
  summarize(votes = sum(votes, na.rm = TRUE)) %>% 
  # reshape so each party vote total is its own variable
  pivot_wider(names_from = party3, values_from = votes) %>%
  # create a ratio variables
  mutate(D = if_else(is.na(D), 0, D),
         R = if_else(is.na(R), 0, R),
         other = if_else(is.na(other), 0, other),
         d_ratio = D / (other + R + D),
         r_ratio = R / (other + R + D)) %>% 
  filter(!is.na(d_ratio))

# modify fips code style to match shape file
df_local_map <- df_local_map %>% mutate(FIPS = str_pad(as.character(county_fips),width = 5,side='left',pad='0'))
saveRDS(df_local_map, 'data/local_map.rds')


### State map
df_state <- read_csv('data/2016-precinct-state.csv')
# make a new variable for democrats, republicans, and lump others together
df_state <- df_state %>% 
  filter(office %in% c('State House','State Senate')) %>%
  mutate(party3 = case_when(party == 'democratic' ~ 'D',
                            party == 'republican' ~ 'R',
                            TRUE ~ 'other'))


df_state_map <- df_state %>% 
  group_by(state, county_name, county_fips, county_lat, county_long, party3) %>% 
  summarize(votes = sum(votes, na.rm = TRUE)) %>% 
  # reshape so each party vote total is its own variable
  pivot_wider(names_from = party3, values_from = votes) %>%
  # create a ratio variables
  mutate(D = if_else(is.na(D), 0, D),
         R = if_else(is.na(R), 0, R),
         other = if_else(is.na(other), 0, other),
         d_ratio = D / (other + R + D),
         r_ratio = R / (other + R + D)) %>% 
  filter(!is.na(d_ratio))

# modify fips code style to match shape file
df_state_map <- df_state_map %>% mutate(FIPS = str_pad(as.character(county_fips),width = 5,side='left',pad='0'))
saveRDS(df_state_map, 'data/state_map.rds')

### President map
df_prez <- read_csv('data/2016-precinct-president.csv')
df_prez <- df_prez %>% mutate(party3 = case_when(party == 'democratic' | candidate == 'Hillary Clinton' ~ 'D',
                                                 party == 'republican' | candidate == 'Donald Trump' ~ 'R',
                                                 TRUE ~ 'other'))


df_prez_map <- df_prez %>% 
  group_by(state, county_name, county_fips, county_lat, county_long, party3) %>% 
  summarize(votes = sum(votes)) %>% 
  # reshape so each party vote total is its own variable
  pivot_wider(names_from = party3, values_from = votes) %>%
  # create a ratio variables
  mutate(d_ratio = D / (other + R + D),
         r_ratio = R / (other + R + D))

# modify fips code style to match shape file
df_prez_map <- df_prez_map %>% mutate(FIPS = str_pad(as.character(county_fips),width = 5,side='left',pad='0'))
saveRDS(df_prez_map, 'data/president_map.rds')