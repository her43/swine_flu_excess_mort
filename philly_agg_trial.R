# SWINE FLU EXCESS MORTALITY 
# PHILADLEPHIA AGGREGATE TRIAL
# HEATHER ROLLINS


library(tidyverse)
#################################################
# MORTALITY DATA IS STORED IN THE ENCRYPTED SERVER IN THE FOLDER:
path_pavital_data <- paste0("//files.drexel.edu/encrypted/SOPH/UHC/",
                            "Wahid_PAVitalStat/Data/")
# CSV FILES
# "CT00deathgeo2000.csv" THROUGH "CT00deathgeo2010.csv"
row_mort <- lapply(2000:2010, function(this_year){
  print(paste0("year ", this_year))
  raw_temp <- read.csv(file = paste0(path_pavital_data,
                                     "CT00deathgeo", this_year, ".csv"),
                       header = T, sep = ",", dec = ".", 
                       colClasses = "character") 
  colnames(raw_temp)<- tolower(colnames(raw_temp))
  raw_temp2 <- raw_temp %>%
    dplyr::select(fileno, uniqueid,  sex, age6011, age_death,
                  hispanic, race1, race9011, geoid10, geoid00) %>%
    dplyr::filter(grepl(x = geoid00, pattern = "42101")) %>%
    dplyr::mutate(age6011 = as.numeric(age6011),
                  age_death = as.numeric(age_death),
                  age = ifelse(age6011 %in% 200:699, 0,
                               ifelse(age6011 %in% 0:150, age6011, 
                                      ifelse(!is.na(age_death), 
                                             floor(age_death), NA))),
                  sex_label = ifelse(sex %in% 1, "male",
                                     ifelse(sex %in% 2, "female", 
                                            "unknown")),
                  male = ifelse(sex %in% 1, 1,
                                ifelse(sex %in% 2, 0, NA)),
                  year = this_year,
                  hispanic = as.numeric(hispanic),
                  hispanic_label = 
                    ifelse(hispanic %in% 1, "non-hispanic",
                           ifelse(hispanic %in% c(2:5), "hispanic", 
                                  "unknown")),
                  hispanic_01 = 
                    ifelse(hispanic %in% 1, 0,
                           ifelse(hispanic %in% c(2:5), 1, 
                                  "unknown")),
                  race9011 = as.numeric(race9011),
                  race_label = 
                    ifelse(race9011 %in% 1, "white",
                           ifelse(race9011 %in% 2, "black",
                                  ifelse(race9011 %in% 3, "native american",
                                         ifelse(race9011 %in% c(4:6, 8, 0),
                                                "asian/pi",
                                                ifelse(race9011 %in% c(7),
                                                       "other","unknown"))))),
                  race_num = 
                    ifelse(race9011 %in% 1, 1,
                           ifelse(race9011 %in% 2, 2,
                                  ifelse(race9011 %in% c(4:6, 8, 0), 3,
                                         ifelse(race9011 %in% 3, 4,
                                                ifelse(race9011 %in% c(7),
                                                       5, NA)))))) %>% 
    dplyr::select(year, fileno, uniqueid, age, sex_label, 
                  male, hispanic_label,hispanic_01,
                  race_label, race_num, geoid10, geoid00 ) 
  return(raw_temp2)
})
