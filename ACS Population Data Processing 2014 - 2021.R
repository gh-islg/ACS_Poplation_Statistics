##############################
# Brian Holliday
# Date : 5/2/23
# Objective:
## Process ACS data pull for 1 Year and 5 Year data
#####
# Change Log
# adding non-hispanic populations from ACS table B03002 8/10/23
#####

###############################

library("dplyr")

get_sjc_sites <- function(df) { # function to identify sjc sites
  # define site_name to fips code dict
  site_names_dict <- c("Ada"="16001", "Shelby"="47157", "Pennington"="46103", "Missoula"="30063",
                       "Minnehaha"="46099", "Pima"="04019", "Charleston"="45019", 
                       "New York City"="36047", # Kings County
                       "New York City"="36061", # New York County
                       "New York City"="36005", # Bronx County
                       "New York City"="36085", # Richmond County    
                       "New York City"="36081", # Queens County
                       "Mecklenburg"="37119", "Los Angeles"="06037",
                       "Allegheny"="42003", "Harris"="48201", "Clark"="32003",
                       "Multnomah"="41051", "Spokane"="53063", "New Orleans"="22071",
                       "Buncombe"="37021", "Cook"="17031", "East Baton Rouge"="22033",
                       "Lake"="17097", "Lucas"="39095", "Milwaukee"="55079",
                       "Missoula"="30063", "Palm Beach"="12099", "Philadelphia"="42101",
                       "San Francisco"="06075", "St. Louis"="29189")
  
  # loop through the dictionary to get site sjc alone
  for (i in seq(1, length(site_names_dict))) {
    insert_rows <- df$statefip_countyfip == site_names_dict[i]
    df[insert_rows, "sjc_site"] <- names(site_names_dict[i])
  }
  return(df)
}

################### process one year data ###############################
# one year acs data
one_year_acs <- readr::read_csv("P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/one_year_acs_raw_data_2014_2021.csv") %>%
  tidyr::separate_wider_delim(., col = "county", delim = ",", names = c("site", "state")) %>%
  dplyr::mutate(site = stringr::str_sub(site, start = 1, end = -7),
                agg_type = ifelse(grepl(pattern = "pop_adult_\\d+_any_ethnicity", x = population_type), "pop_adult_any_ethnicity", NA),
                agg_type = ifelse(grepl(pattern = "pop_adult_white_\\d+_any_ethnicity", x = population_type), "pop_adult_white_any_ethnicity", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_black_\\d+_any_ethnicity", x = population_type), "pop_adult_black_any_ethnicity", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_AIAN_\\d+_any_ethnicity", x = population_type), "pop_adult_AIAN_any_ethnicity", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_A_\\d+_any_ethnicity", x = population_type), "pop_adult_A_any_ethnicity", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_PI_\\d+_any_ethnicity", x = population_type), "pop_adult_PI_any_ethnicity", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_white_\\d+_non_hispanic", x = population_type), "pop_adult_white_non_hispanic", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_hispanic_\\d+_any_race", x = population_type), "pop_adult_hispanic_any_race", agg_type),
                agg_type = ifelse(is.na(agg_type), population_type, agg_type),
                population_type = agg_type,
                population_value = population) %>%
  dplyr::group_by(statefip, countyfip, year, population_type) %>%
  # using reframe instead of summarise
  dplyr::reframe(county_name = site, 
                 state = state,
                 year = year,
                 statefip = statefip,
                 countyfip = countyfip,
                 statefip_countyfip = paste(statefip, countyfip, sep = ""),
                 source = source,
                 population_type = population_type,
                 one_year_value = sum(population_value, na.rm = TRUE)) %>%
  dplyr::distinct(.) %>%  
  get_sjc_sites(.) %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws)) # trimws on all character columns

################### five year acs data ############################
five_year_acs <- readr::read_csv("P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/five_year_acs_raw_data_2014_2021.csv") %>%
  tidyr::separate_wider_delim(., col = "county", delim = ",", names = c("site", "state")) %>%
  dplyr::mutate(site = stringr::str_sub(site, start = 1, end = -7),
                agg_type = ifelse(grepl(pattern = "pop_adult_\\d+_any_ethnicity", x = population_type), "pop_adult_any_ethnicity", NA),
                agg_type = ifelse(grepl(pattern = "pop_adult_white_\\d+_any_ethnicity", x = population_type), "pop_adult_white_any_ethnicity", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_black_\\d+_any_ethnicity", x = population_type), "pop_adult_black_any_ethnicity", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_AIAN_\\d+_any_ethnicity", x = population_type), "pop_adult_AIAN_any_ethnicity", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_A_\\d+_any_ethnicity", x = population_type), "pop_adult_A_any_ethnicity", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_PI_\\d+_any_ethnicity", x = population_type), "pop_adult_PI_any_ethnicity", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_white_\\d+_non_hispanic", x = population_type), "pop_adult_white_non_hispanic", agg_type),
                agg_type = ifelse(grepl(pattern = "pop_adult_hispanic_\\d+_any_race", x = population_type), "pop_adult_hispanic_any_race", agg_type),
                agg_type = ifelse(is.na(agg_type), population_type, agg_type),
                population_type = agg_type,
                population_value = population) %>%
  dplyr::group_by(statefip, countyfip, year, population_type) %>%
  # using reframe instead of summarise
  dplyr::reframe(county_name = site, 
                 state = state,
                 year = year,
                 statefip = statefip,
                 countyfip = countyfip,
                 statefip_countyfip = paste(statefip, countyfip, sep = ""),
                 source = source,
                 population_type = population_type,
                 five_year_value = sum(population_value, na.rm = TRUE)) %>%
  dplyr::distinct(.) %>%  
  get_sjc_sites(.) %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws)) # trimws on all character columns

five_year_acs_non_hispanic <- readr::read_csv("P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/five_year_acs_non_hispanic_raw_data_2014_2021.csv") %>%
  tidyr::separate_wider_delim(., col = "county", delim = ",", names = c("site", "state")) %>%
  dplyr::mutate(site = stringr::str_sub(site, start = 1, end = -7),
                county_name = site,
                statefip_countyfip = paste(statefip, countyfip, sep = ""),
                five_year_value = population) %>%
  dplyr::select(county_name, state, year, statefip,
                countyfip, statefip_countyfip, source, 
                population_type,five_year_value) %>%
  dplyr::distinct(.) %>%  
  get_sjc_sites(.) %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws)) %>% # trimws on all character columns
  dplyr::filter(year != 2020 & !(population_type %in% c("pop_total_any_ethnicity", "pop_total_white_non_hispanic")))

# bind five year regular and non hispanic tables
five_year_acs <- five_year_acs %>%
  rbind(., five_year_acs_non_hispanic)

# full table with each value and each source in long format
one_and_five_year_acs_full_table_long <- one_year_acs %>%
  dplyr::full_join(., five_year_acs, by = c("statefip", "countyfip", "year", "population_type",
                                            "county_name", "state", "statefip_countyfip", "sjc_site")) %>%
  dplyr::mutate(source = ifelse(is.na(source.x), source.y, source.x),
                value = ifelse(!is.na(one_year_value), one_year_value, five_year_value)) %>% # use one year acs first then five year acs
  dplyr::rename("site"="sjc_site") %>%
  dplyr::select(site, year, county_name, state, statefip, countyfip, statefip_countyfip,
                population_type, source.x, source.y, one_year_value, five_year_value) %>%
  # filter out pueto rico
  dplyr::filter(state != "Puerto Rico")

# full table with each source in wide format
one_and_five_year_acs_full_table_wide <- one_year_acs %>%
  dplyr::full_join(., five_year_acs, by = c("statefip", "countyfip", "year", "population_type",
                                            "county_name", "state", "statefip_countyfip", "sjc_site")) %>%
  dplyr::mutate(source = ifelse(is.na(source.x), source.y, source.x),
                value = ifelse(!is.na(one_year_value), one_year_value, five_year_value)) %>% # use one year acs first then five year acs
  dplyr::rename("site"="sjc_site") %>%
  dplyr::select(site, year, county_name, state, statefip, countyfip, statefip_countyfip, 
                population_type, source, value) %>%
  tidyr::pivot_wider(., names_from = "population_type", values_from = "value") %>%
  dplyr::arrange(state, county_name, year, source) %>%
  dplyr::select(site, year, county_name, state, statefip, 
                countyfip, statefip_countyfip, source, 
                # total
                pop_total_any_ethnicity, pop_total_non_hispanic,
                pop_adult_any_ethnicity,
                # hispanic
                pop_total_hispanic_any_race, pop_adult_hispanic_any_race,
                # white
                pop_total_white_any_ethnicity, pop_adult_white_any_ethnicity, 
                pop_total_white_non_hispanic, pop_adult_white_non_hispanic,
                # black
                pop_total_black_any_ethnicity, pop_adult_black_any_ethnicity,
                pop_total_black_non_hispanic,
                # AIAN
                pop_total_AIAN_any_ethnicity, pop_adult_AIAN_any_ethnicity,
                pop_total_AIAN_non_hispanic,
                # asian
                pop_total_A_any_ethnicity, pop_adult_A_any_ethnicity,
                pop_total_A_non_hispanic,
                # pacific islander
                pop_total_PI_any_ethnicity, pop_adult_PI_any_ethnicity,
                pop_total_PI_non_hispanic) %>%
  dplyr::filter(state != "Puerto Rico")

# full table with each source in wide format
one_and_five_year_acs_wide <-  one_year_acs %>%
  dplyr::full_join(., five_year_acs, by = c("statefip", "countyfip", "year", "population_type",
                                            "county_name", "state", "statefip_countyfip", "sjc_site")) %>%
  dplyr::mutate(source = paste(as.character(year), "1/5 Year ACS"),
                value = ifelse(!is.na(one_year_value), one_year_value, five_year_value)) %>% # use one year acs first then five year acs
  dplyr::rename("site"="sjc_site") %>%
  dplyr::select(site, year, county_name, state, statefip, countyfip, statefip_countyfip, 
                population_type, source, value) %>%
  tidyr::pivot_wider(., names_from = "population_type", values_from = "value") %>%
  dplyr::mutate(pop_total_API_any_ethnicity = pop_total_A_any_ethnicity + pop_total_PI_any_ethnicity,
                pop_adult_API_any_ethnicity = pop_adult_A_any_ethnicity + pop_adult_PI_any_ethnicity,
                pop_total_API_non_hispanic = pop_total_A_non_hispanic + pop_total_PI_non_hispanic,
                pop_total_poc = pop_total_any_ethnicity - pop_total_white_any_ethnicity,
                pop_adult_poc = pop_adult_any_ethnicity - pop_adult_white_any_ethnicity) %>%
  dplyr::select(site, year, county_name, state, statefip, 
                countyfip, statefip_countyfip, source,
                # total
                pop_total_any_ethnicity, pop_adult_any_ethnicity,
                pop_total_non_hispanic,
                # POC
                pop_total_poc, pop_adult_poc,
                # hispanic
                pop_total_hispanic_any_race, pop_adult_hispanic_any_race,
                # white
                pop_total_white_any_ethnicity, pop_adult_white_any_ethnicity, 
                pop_total_white_non_hispanic, pop_adult_white_non_hispanic, 
                # black
                pop_total_black_any_ethnicity, pop_adult_black_any_ethnicity,
                pop_total_black_non_hispanic,
                # AIAN
                pop_total_AIAN_any_ethnicity, pop_adult_AIAN_any_ethnicity,
                pop_total_AIAN_non_hispanic,
                # API
                pop_total_API_any_ethnicity, pop_adult_API_any_ethnicity,
                pop_total_API_non_hispanic,
                # asian
                pop_total_A_any_ethnicity, pop_adult_A_any_ethnicity,
                pop_total_A_non_hispanic,
                # pacific islander
                pop_total_PI_any_ethnicity, pop_adult_PI_any_ethnicity,
                pop_total_PI_non_hispanic) %>%
  dplyr::arrange(site, year) %>%
  dplyr::filter(state != "Puerto Rico")


# abbreviated full table in wide format
one_and_five_year_acs_wide_sjc_only <- one_year_acs %>%
  dplyr::full_join(., five_year_acs, by = c("statefip", "countyfip", "year", "population_type",
                                            "county_name", "state", "statefip_countyfip", "sjc_site")) %>%
  dplyr::mutate(source = paste(as.character(year), "1/5 Year ACS"),
                value = ifelse(!is.na(one_year_value), one_year_value, five_year_value)) %>% # use one year acs first then five year acs
  dplyr::rename("site"="sjc_site") %>%
  dplyr::select(site, year, county_name, state, statefip, countyfip, statefip_countyfip, 
                population_type, source, value) %>%
  tidyr::pivot_wider(., names_from = "population_type", values_from = "value") %>%
  dplyr::filter(!(is.na(site))) %>%
  dplyr::mutate(pop_total_API_any_ethnicity = pop_total_A_any_ethnicity + pop_total_PI_any_ethnicity,
                pop_adult_API_any_ethnicity = pop_adult_A_any_ethnicity + pop_adult_PI_any_ethnicity,
                pop_total_API_non_hispanic = pop_total_A_non_hispanic + pop_total_PI_non_hispanic,
                pop_total_poc = pop_total_any_ethnicity - pop_total_white_any_ethnicity,
                pop_adult_poc = pop_adult_any_ethnicity - pop_adult_white_any_ethnicity) %>%
  dplyr::select(site, year, county_name, state, statefip, 
                countyfip, statefip_countyfip, source,
                # total
                pop_total_any_ethnicity, pop_adult_any_ethnicity,
                pop_total_non_hispanic,
                # POC
                pop_total_poc, pop_adult_poc,
                # hispanic
                pop_total_hispanic_any_race, pop_adult_hispanic_any_race,
                # white
                pop_total_white_any_ethnicity, pop_adult_white_any_ethnicity, 
                pop_total_white_non_hispanic, pop_adult_white_non_hispanic, 
                # black
                pop_total_black_any_ethnicity, pop_adult_black_any_ethnicity,
                pop_total_black_non_hispanic,
                # AIAN
                pop_total_AIAN_any_ethnicity, pop_adult_AIAN_any_ethnicity,
                pop_total_AIAN_non_hispanic,
                # API
                pop_total_API_any_ethnicity, pop_adult_API_any_ethnicity,
                pop_total_API_non_hispanic,
                # asian
                pop_total_A_any_ethnicity, pop_adult_A_any_ethnicity,
                pop_total_A_non_hispanic,
                # pacific islander
                pop_total_PI_any_ethnicity, pop_adult_PI_any_ethnicity,
                pop_total_PI_non_hispanic) %>%
  dplyr::arrange(site, year) %>%
  dplyr::filter(state != "Puerto Rico")

# aggregate NYC
nyc_sjc <- one_and_five_year_acs_wide_sjc_only %>%
  dplyr::filter(site == "New York City") %>%
  tidyr::pivot_longer(., cols = dplyr::starts_with("pop"), names_to = "population_type", values_to = "value") %>%
  dplyr::group_by(year, population_type) %>%
  dplyr::reframe(site = site,
                 year = year,
                 county_name = NA,
                 state = state,
                 statefip = statefip,
                 countyfip = NA,
                 statefip_countyfip = NA,
                 source = source,
                 population_type = population_type,
                 value = sum(value, na.rm = TRUE)) %>%
  dplyr::distinct(.) %>%
  tidyr::pivot_wider(., names_from = "population_type", values_from = "value")

nyc_sjc <- nyc_sjc %>%
  rbind(., nyc_sjc %>%
          dplyr::filter(year == 2019) %>%
          dplyr::mutate(year = 2020))

# row bind with nyc and add 2020
one_and_five_year_acs_wide <- one_and_five_year_acs_wide %>%
  rbind(., one_and_five_year_acs_wide %>%
          dplyr::filter(year == 2019) %>%
          dplyr::mutate(year = 2020)) %>%
  dplyr::mutate(site = ifelse(site == "New York City", NA, site))

# row bind with nyc and add 2020
one_and_five_year_acs_wide_sjc_only <- one_and_five_year_acs_wide_sjc_only %>%
  rbind(., one_and_five_year_acs_wide_sjc_only %>%
          dplyr::filter(year == 2019) %>%
          dplyr::mutate(year = 2020))

# add nyc back to the dataframe
one_and_five_year_acs_wide <- one_and_five_year_acs_wide %>%
  rbind(., nyc_sjc) %>%
  dplyr::arrange(site, year)

# add nyc back to the dataframe
one_and_five_year_acs_wide_sjc_only <- one_and_five_year_acs_wide_sjc_only %>%
  dplyr::filter(!(site == "New York City")) %>%
  rbind(., nyc_sjc) %>%
  dplyr::arrange(site, year)

# write files 
write.csv(one_and_five_year_acs_full_table_long, # write long file full sources
          file = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/outputs/csv/2014 - 2021 ACS Population Data full sources (SJC and US Counties) long.csv",
          fileEncoding = "UTF-8", row.names = FALSE)

write.csv(one_and_five_year_acs_full_table_wide, # write wide file full sources
          file = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/outputs/csv/2014 - 2021 ACS Population Data full sources (SJC and US Counties) wide.csv",
          fileEncoding = "UTF-8", row.names = FALSE)

write.csv(one_and_five_year_acs_wide, # write regular wide file
          file = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/outputs/csv/2014 - 2021 ACS Population Data (SJC and US Counties).csv",
          fileEncoding = "UTF-8", row.names = FALSE)

write.csv(one_and_five_year_acs_wide_sjc_only, # write regular wide file
          file = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/outputs/csv/2014 - 2021 ACS Population Data (SJC Only).csv",
          fileEncoding = "UTF-8", row.names = FALSE)

writexl::write_xlsx(one_and_five_year_acs_full_table_long, # write long file full sources
                    path = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/outputs/excel/2014 - 2021 ACS Population Data full sources (SJC and US Counties) long.xlsx")

writexl::write_xlsx(one_and_five_year_acs_full_table_wide, # write wide file full sources
                    path = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/outputs/excel/2014 - 2021 ACS Population Data full sources (SJC and US Counties) wide.xlsx")

writexl::write_xlsx(one_and_five_year_acs_wide, # write regular wide file
                    path = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/outputs/excel/2014 - 2021 ACS Population Data (SJC and US Counties).xlsx")

writexl::write_xlsx(one_and_five_year_acs_wide_sjc_only, # write regular wide file
                    path = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/outputs/excel/2014 - 2021 ACS Population Data (SJC Only).xlsx")
