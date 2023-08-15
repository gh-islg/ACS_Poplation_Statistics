###############################
# Brian Holliday
# Date : 5/2/23
# Objective:
## Import ACS data templates using census API key for 2014 - 2021 populations
###############################


################## One Year ACS data pull #######################
library("dplyr")

census_api_key <- readr::read_csv("C:/Users/Brian.Holliday/Documents/brian holliday api keys.csv") %>%
  dplyr::filter(key_type == "US Census") %>%
  dplyr::select(api_key) %>%
  unlist(.)

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

# make total emtpy tables
one_year_acs_api <- data.frame(county = character(),
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
    for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
      
      print(paste(as.character(pop_type), as.character(year)))
      
      # initialize URL for each year and race population
      url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs1?get=NAME,B01001", pop_type, "_001E&for=county:*&in=state:*&key=", census_api_key, sep = "")
      response <- httr::GET(url) # get response from url
    
      if (httr::status_code(response) == 200) { # 200 means good response  
    
      if (pop_type == "") {
        current_pop_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_any_ethnicity")
      
        one_year_acs_api <- one_year_acs_api %>%
          rbind(., current_pop_total)
      
      } else if (pop_type == "A") {
        current_white_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_white_any_ethnicity")
      
        one_year_acs_api <- one_year_acs_api %>%
          rbind(., current_white_alone_total)
      
      } else if (pop_type == "B") {
        current_black_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_black_any_ethnicity")
      
        one_year_acs_api <- one_year_acs_api %>%
          rbind(., current_black_alone_total)
      
      } else if (pop_type == "C") {
        current_aian_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_AIAN_any_ethnicity") 
      
        one_year_acs_api <- one_year_acs_api %>%
          rbind(., current_aian_alone_total)
      
      } else if (pop_type == "D") {
        current_asian_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_A_any_ethnicity")
      
        one_year_acs_api <- one_year_acs_api %>%
          rbind(., current_asian_alone_total)
      
      } else if (pop_type == "E") {
        current_pacific_islander_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_PI_any_ethnicity")
      
        one_year_acs_api <- one_year_acs_api %>%
          rbind(., current_pacific_islander_total)
      
      } else if (pop_type == "H") {
        current_white_non_hispanic_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_white_non_hispanic")
      
        one_year_acs_api <- one_year_acs_api %>%
          rbind(current_white_non_hispanic_total)
      
      } else if (pop_type == "I") {
        current_hispanic_any_race <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_hispanic_any_race")
      
        one_year_acs_api <- one_year_acs_api %>%
          rbind(., current_hispanic_any_race)
      }
  } else {
    next # if request is bad we move to the next population
    print(paste("Error:", httr::status_code(response), pop_type))
    }
  }
}

############## total population 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:49) {
    if (i %in% c(26:30)) {
        next # we have to skip table 26
      }
    
    print(paste("total adult 18+", as.character(year)))
      
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs1?get=NAME,B01001", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_", table_num, "_any_ethnicity", sep = ""))
      
      one_year_acs_api <- one_year_acs_api %>%
        rbind(., current_pop_total)
        
    } else {
      print(paste("Error:", httr::status_code(response), "total pop 18+"))
      next # if request is bad we move to the next population
        
    }
  }
}
##########################################################

############## white alone 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 26
    }
    
    print(paste("White alone", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs1?get=NAME,B01001A", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_white_", table_num, "_any_ethnicity", sep = ""))
      
      one_year_acs_api <- one_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "white pop 18+"))
    }
  }
}

############## black alone 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17
    }
    
    print(paste("Black alone", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs1?get=NAME,B01001B", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_black_", table_num, "_any_ethnicity", sep = ""))
      
      one_year_acs_api <- one_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "black pop 18+"))
    }
  }
}

############## AIAN alone 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17
    }
    
    print(paste("AIAN alone", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs1?get=NAME,B01001C", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_AIAN_", table_num, "_any_ethnicity", sep = ""))
      
      one_year_acs_api <- one_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "black pop 18+"))
    }
  }
}

############## Asian alone 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17
    }
    
    print(paste("Asian alone", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs1?get=NAME,B01001D", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_A_", table_num, "_any_ethnicity", sep = ""))
      
      one_year_acs_api <- one_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "Asian pop 18+"))
    }
  }
}

############## Pacific Islander alone 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17, 18, 19, 20, 21
    }
    
    print(paste("Pacific Islander alone", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs1?get=NAME,B01001E", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_PI_", table_num, "_any_ethnicity", sep = ""))
      
      one_year_acs_api <- one_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "Pacific Islander pop 18+"))
    }
  }
}

############## White alone non hispanic 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17
    }
    
    print(paste("White alone non hispanic", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs1?get=NAME,B01001H", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_white_", table_num, "_non_hispanic", sep = ""))
      
      one_year_acs_api <- one_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "White non hispanic pop 18+"))
    }
  }
}

############## Hispanic any ethnicity 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17
    }
    
    print(paste("Hispanic any ethnicity", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs1?get=NAME,B01001I", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_hispanic_", table_num, "_any_race", sep = ""))
      
      one_year_acs_api <- one_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "Hispanic any race pop 18+"))
    }
  }
}

############# Five Year ACS Data Pull ################################
# make total emtpy tables
five_year_acs_api <- data.frame(county = character(),
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
  for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
    
    print(paste(as.character(pop_type), as.character(year)))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,B01001", pop_type, "_001E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      
      if (pop_type == "") {
        current_pop_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_any_ethnicity")
        
        five_year_acs_api <- five_year_acs_api %>%
          rbind(., current_pop_total)
        
      } else if (pop_type == "A") {
        current_white_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_white_any_ethnicity")
        
        five_year_acs_api <- five_year_acs_api %>%
          rbind(., current_white_alone_total)
        
      } else if (pop_type == "B") {
        current_black_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_black_any_ethnicity")
        
        five_year_acs_api <- five_year_acs_api %>%
          rbind(., current_black_alone_total)
        
      } else if (pop_type == "C") {
        current_aian_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_AIAN_any_ethnicity") 
        
        five_year_acs_api <- five_year_acs_api %>%
          rbind(., current_aian_alone_total)
        
      } else if (pop_type == "D") {
        current_asian_alone_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_A_any_ethnicity")
        
        five_year_acs_api <- five_year_acs_api %>%
          rbind(., current_asian_alone_total)
        
      } else if (pop_type == "E") {
        current_pacific_islander_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_PI_any_ethnicity")
        
        five_year_acs_api <- five_year_acs_api %>%
          rbind(., current_pacific_islander_total)
        
      } else if (pop_type == "H") {
        current_white_non_hispanic_total <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_white_non_hispanic")
        
        five_year_acs_api <- five_year_acs_api %>%
          rbind(current_white_non_hispanic_total)
        
      } else if (pop_type == "I") {
        current_hispanic_any_race <- httr::content(response) %>%
          process_census_api_call(., as.numeric(year), "total", "pop_total_hispanic_any_race")
        
        five_year_acs_api <- five_year_acs_api %>%
          rbind(., current_hispanic_any_race)
      }
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), pop_type))
    }
  }
}

############## total population 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:49) {
    if (i %in% c(26:30)) {
      next # we have to skip table 26
    }
    
    print(paste("total adult 18+", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,B01001", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_", table_num, "_any_ethnicity", sep = ""))
      
      five_year_acs_api <- five_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "total pop 18+"))
    }
  }
}

############## white alone 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 26
    }
    
    print(paste("White alone", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,B01001A", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_white_", table_num, "_any_ethnicity", sep = ""))
      
      five_year_acs_api <- five_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "white pop 18+"))
    }
  }
}

############## black alone 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17
    }
    
    print(paste("Black alone", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,B01001B", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_black_", table_num, "_any_ethnicity", sep = ""))
      
      five_year_acs_api <- five_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "black pop 18+"))
    }
  }
}

############## AIAN alone 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17
    }
    
    print(paste("AIAN alone", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,B01001C", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_AIAN_", table_num, "_any_ethnicity", sep = ""))
      
      five_year_acs_api <- five_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "black pop 18+"))
    }
  }
}

############## Asian alone 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17
    }
    
    print(paste("Asian alone", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,B01001D", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_A_", table_num, "_any_ethnicity", sep = ""))
      
      five_year_acs_api <- five_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "Asian pop 18+"))
    }
  }
}

############## Pacific Islander alone 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17
    }
    
    print(paste("Pacific Islander alone", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,B01001E", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_PI_", table_num, "_any_ethnicity", sep = ""))
      
      five_year_acs_api <- five_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "Pacific Islander pop 18+"))
    }
  }
}

############## White alone non hispanic 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17
    }
    
    print(paste("White alone non hispanic", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,B01001H", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_white_", table_num, "_non_hispanic", sep = ""))
      
      five_year_acs_api <- five_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      next # if request is bad we move to the next population
      print(paste("Error:", httr::status_code(response), "White non hispanic pop 18+"))
    }
  }
}

############## Hispanic any ethnicity 18+ all years ################
for (year in c("2014", "2015", "2016", "2017", "2018", "2019", "2021")) {
  
  for (i in 7:31) {
    if (i %in% c(17:21)) {
      next # we have to skip table 17
    }
    
    print(paste("Hispanic any ethnicity", as.character(year)))
    
    table_num <- ifelse(i > 9, paste("0", as.character(i), sep = ""), paste("00", as.character(i), sep = ""))
    
    # initialize URL for each year and race population
    url <- paste("https://api.census.gov/data/", as.character(year), "/acs/acs5?get=NAME,B01001I", "_", table_num, "E&for=county:*&in=state:*&key=", census_api_key, sep = "")
    response <- httr::GET(url) # get response from url
    
    if (httr::status_code(response) == 200) { # 200 means good response  
      current_pop_total <- httr::content(response) %>%
        process_census_api_call(., as.numeric(year), "18+", paste("pop_adult_hispanic_", table_num, "_any_race", sep = ""))
      
      five_year_acs_api <- five_year_acs_api %>%
        rbind(., current_pop_total)
      
    } else {
      print(paste("Error:", httr::status_code(response), "Hispanic any race pop 18+"))
      next # if request is bad we move to the next population
    }
  }
}

one_year_acs_api <- one_year_acs_api %>%
  dplyr::mutate(source = paste(as.character(year), "1 Year ACS"))

five_year_acs_api <- five_year_acs_api %>%
  dplyr::mutate(source = paste(as.character(year), "5 Year ACS"))

# output data
write.csv(one_year_acs_api, file = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/one_year_acs_raw_data_2014_2021.csv",
          row.names = FALSE, fileEncoding = "UTF-8", na = "")

write.csv(five_year_acs_api, file = "P:/SJC/03. Data and PM Management/Population Data/ACS Data Processing/five_year_acs_raw_data_2014_2021.csv",
          row.names = FALSE, fileEncoding = "UTF-8", na = "")


