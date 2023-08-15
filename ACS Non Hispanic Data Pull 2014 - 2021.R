###############################
# Brian Holliday
# Date : 8/8/23
# Objective:
## Import ACS data for non-hispanic by race using census API key for 2014 - 2021 populations
###############################

library("dplyr")

process_census_api_call <- function(x, current_year, age_group, current_population_type) {
  # initialize empty dataframe
  df <- data.frame(county = character(),
                   year = numeric(),
                   population = numeric(), 
                   statefip = character(), 
                   countyfip = character(),
                   population_type = character(),
                   age = character())
  # loop through rows from the api call
  for (i in 2:length(x)) {
    county_name <- x[[i]][[1]]
    county_population <- as.numeric(x[[i]][[2]])
    current_statefip <- x[[i]][[3]]
    current_countyfip <- x[[i]][[4]]
    
    df <- df %>%
      tibble::add_row(county = county_name,
                      year = current_year,
                      population = county_population,
                      statefip = current_statefip,
                      countyfip = current_countyfip,
                      population_type = current_population_type,
                      age = age_group)
  }
  return(df)
}

census_api_key <- readr::read_csv("C:/Users/Brian.Holliday/Documents/brian holliday api keys.csv") %>% # this census api key is unique to me and you may have to request a unique one.
  dplyr::filter(key_type == "US Census") %>%
  dplyr::select(api_key) %>%
  unlist(.) 

############# Five Year ACS Data Pull ################################
# make total emtpy tables
five_year_acs_api_any_ethnicity <- data.frame(county = character(),
                                              year = numeric(),
                                              population = numeric(), 
                                              statefip = character(), 
                                              countyfip = character(),
                                              population_type = character(),
                                              age = character())

############## total population all years ################
for (pop_type in c("", # total population
                   "A", # White alone
                   "B", # Black alone
                   "C", # AIAN alone
                   "D", # Asian alone
                   "E", # Native Hawaiian Pacfic Islander alone
                   "H", # White alone, Not Hispanic or Latino
                   "I" # Hispanic or Latino
)) {
  for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")) {
    
    print(paste(as.character(pop_type), as.character(year)))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,B01001", pop_type, "_001E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      
      if (pop_type == "") {
        current_pop_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_any_ethnicity")
        
        five_year_acs_api_any_ethnicity <- five_year_acs_api_any_ethnicity %>%
          rbind(., current_pop_total)
        
      } else if (pop_type == "A") {
        current_white_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_white_any_ethnicity")
        
        five_year_acs_api_any_ethnicity <- five_year_acs_api_any_ethnicity %>%
          rbind(., current_white_alone_total)
        
      } else if (pop_type == "B") {
        current_black_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_black_any_ethnicity")
        
        five_year_acs_api_any_ethnicity <- five_year_acs_api_any_ethnicity %>%
          rbind(., current_black_alone_total)
        
      } else if (pop_type == "C") {
        current_aian_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_AIAN_any_ethnicity") 
        
        five_year_acs_api_any_ethnicity <- five_year_acs_api_any_ethnicity %>%
          rbind(., current_aian_alone_total)
        
      } else if (pop_type == "D") {
        current_asian_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_A_any_ethnicity")
        
        five_year_acs_api_any_ethnicity <- five_year_acs_api_any_ethnicity %>%
          rbind(., current_asian_alone_total)
        
      } else if (pop_type == "E") {
        current_pacific_islander_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_PI_any_ethnicity")
        
        five_year_acs_api_any_ethnicity <- five_year_acs_api_any_ethnicity %>%
          rbind(., current_pacific_islander_total)
        
      } else if (pop_type == "H") {
        current_white_non_hispanic_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_white_non_hispanic")
        
        five_year_acs_api_any_ethnicity <- five_year_acs_api_any_ethnicity %>%
          rbind(current_white_non_hispanic_total)
        
      } else if (pop_type == "I") {
        current_hispanic_any_race <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_hispanic_any_race")
        
        five_year_acs_api_any_ethnicity <- five_year_acs_api_any_ethnicity %>%
          rbind(., current_hispanic_any_race)
      }
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), pop_type))
    }
  }
}

# make total emtpy table
five_year_acs_api_non_hispanic <- data.frame(county = character(),
                               year = numeric(),
                               population = numeric(), 
                               statefip = character(), 
                               countyfip = character(),
                               population_type = character(),
                               age = character())

for (year in 2014:2021) {
  for (i in 1:17) {
    
    if (i %in% 1:7) {
      
      url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,", "B03002", "_00", as.character(i),
                   "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
      
    } else if (i %in% 8:11) {
      
      next
      
    } else if (i %in% 12:17) {
      
      url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,", "B03002", "_0", as.character(i),
                   "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
      
    }
    

    response <- httr::GET(url) # get response from url
    
    print(paste(as.character(i), as.character(year)))
    
    if (httr::status_code(response) == 200) {
      
      if (i == 1) {
        five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
          rbind(., httr::content(response) %>%
                  process_census_api_call(., as.numeric(year), "total", "pop_total_any_ethnicity"))
      }
      
      else if (i == 2) {
        
        five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
          rbind(., httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_non_hispanic"))
      
    } else if (i == 3) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
                process_census_api_call(., as.numeric(year), "total", "pop_total_white_non_hispanic"))
      
    } else if (i == 4) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "total", "pop_total_black_non_hispanic"))
      
    } else if (i == 5) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "total", "pop_total_AIAN_non_hispanic"))
      
    } else if (i == 6) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "total", "pop_total_A_non_hispanic"))
      
    } else if (i == 7) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "total", "pop_total_PI_non_hispanic"))
      
    } else if (i == 8) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
                process_census_api_call(., as.numeric(year), "total", "pop_total_PI_non_hispanic"))      
      
    } else if (i == 12) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
                process_census_api_call(., as.numeric(year), "total", "pop_total_hispanic_any_race"))      
      
    } else if (i == 13) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
                process_census_api_call(., as.numeric(year), "total", "pop_total_white_hispanic_only")) 
      
    } else if (i == 14) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
                process_census_api_call(., as.numeric(year), "total", "pop_total_black_hispanic_only")) 
      
    } else if (i == 15) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
                process_census_api_call(., as.numeric(year), "total", "pop_total_AIAN_hispanic_only")) 
      
    } else if (i == 16) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
                process_census_api_call(., as.numeric(year), "total", "pop_total_A_hispanic_only")) 
      
    } else if (i == 17) {
      
      five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
        rbind(., httr::content(response) %>%
                process_census_api_call(., as.numeric(year), "total", "pop_total_PI_hispanic_only")) 
      } else {
        
        print(paste("Error:", httr::status_code(response), pop_type))
        break # exit loop if bad request
      }
    }
  }
}

five_year_acs_api_any_ethnicity <- five_year_acs_api_any_ethnicity %>%
  dplyr::mutate(source = paste(as.character(year), "5 Year ACS"))

five_year_acs_api_non_hispanic <- five_year_acs_api_non_hispanic %>%
  dplyr::mutate(source = paste(as.character(year), "5 Year ACS"))

five_year_acs <- five_year_acs_api_any_ethnicity %>%
  rbind(., five_year_acs_api_non_hispanic)

# output data
write.csv(five_year_acs, file = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/five_year_acs_non_hispanic_raw_data_2014_2021.csv",
          row.names = FALSE, fileEncoding = "UTF-8", na = "")

write.csv(five_year_acs_api_any_ethnicity, file = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/five_year_acs_non_hispanic_raw_data_2014_2021.csv",
          row.names = FALSE, fileEncoding = "UTF-8", na = "")


