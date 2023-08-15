##############################
# Brian Holliday
# Date : 8/10/23
# Objective:
## Ad Hoc Request for Stephanie to pull 2020 non - hispanic race populations
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

five_year_acs_non_hispanic_wide <- readr::read_csv("P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/five_year_acs_non_hispanic_raw_data_2014_2021.csv") %>%
  tidyr::separate_wider_delim(., col = "county", delim = ",", names = c("site", "state")) %>%
  dplyr::mutate(site = stringr::str_sub(site, start = 1, end = -7),
                county_name = site,
                statefip_countyfip = paste(statefip, countyfip, sep = ""),
                five_year_value = population) %>%
  dplyr::select(county_name, state, year, statefip,
                countyfip, statefip_countyfip, source, 
                population_type, five_year_value) %>%
  dplyr::distinct(.) %>%  
  get_sjc_sites(.) %>%
  dplyr::filter(year == 2020) %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws)) %>% # trimws on all character columns
  tidyr::pivot_wider(., names_from = "population_type", values_from = "five_year_value") %>%
  dplyr::mutate(pop_total_API_any_ethnicity = pop_total_A_any_ethnicity + pop_total_PI_any_ethnicity,
                pop_total_API_non_hispanic = pop_total_A_non_hispanic + pop_total_PI_non_hispanic,
                pop_total_API_hispanic_only = pop_total_A_hispanic_only + pop_total_PI_hispanic_only) %>%
  dplyr::select(sjc_site, year, state, county_name, statefip, countyfip, statefip_countyfip, source,
                pop_total_any_ethnicity, pop_total_non_hispanic, 
                pop_total_hispanic_any_race,
                pop_total_white_any_ethnicity, pop_total_white_non_hispanic, pop_total_white_hispanic_only,
                pop_total_black_any_ethnicity, pop_total_black_non_hispanic, pop_total_black_hispanic_only,
                pop_total_AIAN_any_ethnicity, pop_total_AIAN_non_hispanic, pop_total_AIAN_hispanic_only,
                pop_total_API_any_ethnicity, pop_total_API_non_hispanic, pop_total_API_hispanic_only,
                pop_total_A_any_ethnicity, pop_total_A_non_hispanic, pop_total_A_hispanic_only,
                pop_total_PI_any_ethnicity, pop_total_PI_non_hispanic, pop_total_PI_hispanic_only)

nyc <- five_year_acs_non_hispanic_wide %>%
  dplyr::filter(sjc_site == "New York City") %>%
  tidyr::pivot_longer(., cols = dplyr::starts_with("pop"), names_to = "population_type") %>%
  dplyr::group_by(sjc_site, year, population_type) %>%
  dplyr::reframe(sjc_site = "New York City",
                 year = year,
                 state = state,
                 county_name = NA,
                 statefip = statefip,
                 countyfip = NA,
                 statefip_countyfip = NA,
                 source = source,
                 population_type = population_type,
                 value = sum(value, na.rm = TRUE)) %>%
  dplyr::distinct(.) %>%
  tidyr::pivot_wider(., names_from = "population_type", values_from = "value")

five_year_acs_non_hispanic_wide <- five_year_acs_non_hispanic_wide %>%
  dplyr::filter(!(sjc_site == "New York City") | is.na(sjc_site)) %>%
  rbind(., nyc) %>%
  dplyr::arrange(sjc_site)

write.csv(five_year_acs_non_hispanic_wide, file = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/outputs/csv/2020 ACS Non Hispanic Populations (US Counties).csv",
          row.names = FALSE, fileEncoding = "UTF-8")

write.csv(five_year_acs_non_hispanic_wide %>%
            dplyr::filter(!(is.na(sjc_site))), file = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/outputs/csv/2020 ACS Non Hispanic Populations (SJC Only).csv",
          row.names = FALSE, fileEncoding = "UTF-8")

