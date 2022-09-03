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
raw_mort <- lapply(2000:2010, function(this_year){
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

raw_mort <- do.call(rbind, raw_mort)

#### CENSUS DATA
# THERE IS SOME SAVED CENSUS DATA 
# 2000 CENSUS TRACTS, PA, 2000 CENSUS:        Census2000_PA.csv
# 2000 CENSUS TRACTS, PA 5-YEAR ACS 2005-09:  ACS2005_2009_PA.csv
# 2010 CENSUS TRACTS, PA 5-YEAR ACS 2006-10:  ACS2005_2009_PA.csv

census2000 <- read.csv(file = paste0(path_pavital_data, "Census2000_PA.csv"),
                       header = T)
colnames(census2000) <- tolower(colnames(census2000))
philly_cen2000 <- census2000 %>%
  filter(state %in% 42 & county %in% 101) %>%
  select(geoid00, 
         inc_medhh,
         total_pop        = p012001,
         male_pop         = p012002,
         female_pop       = p012026,
         male_age_00to04  = p012003,
         male_age_05to09  = p012004,
         male_age_10to14  = p012005,
         male_age_15to17  = p012006,
         male_age_18to19  = p012007,
         male_age_20      = p012008,
         male_age_21      = p012009,
         male_age_22to24  = p012010,
         male_age_25to29  = p012011,
         male_age_30to34  = p012012,
         male_age_35to39  = p012013,
         male_age_40to44  = p012014,
         male_age_45to49  = p012015,
         male_age_50to54  = p012016,
         male_age_55to59  = p012017,
         male_age_60to61  = p012018,
         male_age_62to64  = p012019,
         male_age_65to66  = p012020,
         male_age_67to69  = p012021,
         male_age_70to74  = p012022,
         male_age_75to79  = p012023,
         male_age_80to84  = p012024,
         male_age_85plus  = p012025,
         female_age_00to04= p012027,
         female_age_05to09= p012028,
         female_age_10to14= p012029,
         female_age_15to17= p012030,
         female_age_18to19= p012031,
         female_age_20    = p012032,
         female_age_21    = p012033,
         female_age_22to24= p012034,
         female_age_25to29= p012035,
         female_age_30to34= p012036,
         female_age_35to39= p012037,
         female_age_40to44= p012038,
         female_age_45to49= p012039,
         female_age_50to54= p012040,
         female_age_55to59= p012041,
         female_age_60to61= p012042,
         female_age_62to64= p012043,
         female_age_65to66= p012044,
         female_age_67to69= p012045,
         female_age_70to74= p012046,
         female_age_75to79= p012047,
         female_age_80to84= p012048,
         female_age_85plus= p012049)

######################################
# CREATE AGGREGATED DATASETS

# PHILLY, AGGREGATE ALL CENSUS TRACTS, AGE BY SEX (BY YEAR)

# MORTALITY DATA:

philly_mort_asy <- raw_mort %>%
  dplyr::filter(substr(geoid00, 1, 5) %in% "42101") %>%
  dplyr::filter(male %in% 0:1 & !is.na(age)) %>%
  dplyr::mutate(age_group = 
                  case_when(age %in% 0:4   ~ "00to04",
                            age %in% 5:9   ~ "05to09",
                            age %in% 10:14 ~ "10to14",
                            age %in% 15:17 ~ "15to17",
                            age %in% 18:19 ~ "18to19",
                            age %in% 20    ~ "20",
                            age %in% 21    ~ "21",
                            age %in% 22:24 ~ "22to24",
                            age %in% 25:29 ~ "25to29",
                            age %in% 30:34 ~ "30to34",
                            age %in% 35:39 ~ "35to39",
                            age %in% 40:44 ~ "40to44",
                            age %in% 45:49 ~ "45to49",
                            age %in% 50:54 ~ "50to54",
                            age %in% 55:59 ~ "55to59",
                            age %in% 60:61 ~ "60to61",
                            age %in% 62:64 ~ "62to64",
                            age %in% 65:66 ~ "65to66",
                            age %in% 67:69 ~ "67to69",
                            age %in% 70:74 ~ "70to74",
                            age %in% 75:79 ~ "75to79",
                            age %in% 80:84 ~ "80to84",
                            age %in% 85:125~ "85plus"
                  ))%>%
  dplyr::group_by(year, sex_label, male, age_group) %>%
  dplyr::summarize(deaths = n()) %>%
  ungroup()


# 2000 CENSUS DATA:
philly_cen2000_long <- 
  reshape2::melt(data = philly_cen2000,
                 id.vars = "geoid00",
                 measure.vars = c("male_age_00to04",
                                  "male_age_05to09",
                                  "male_age_10to14",
                                  "male_age_15to17",
                                  "male_age_18to19",
                                  "male_age_20",
                                  "male_age_21",
                                  "male_age_22to24",
                                  "male_age_25to29",
                                  "male_age_30to34",
                                  "male_age_35to39",
                                  "male_age_40to44",
                                  "male_age_45to49",
                                  "male_age_50to54",
                                  "male_age_55to59",
                                  "male_age_60to61",
                                  "male_age_62to64",
                                  "male_age_65to66",
                                  "male_age_67to69",
                                  "male_age_70to74",
                                  "male_age_75to79",
                                  "male_age_80to84",
                                  "male_age_85plus",
                                  "female_age_00to04",
                                  "female_age_05to09",
                                  "female_age_10to14",
                                  "female_age_15to17",
                                  "female_age_18to19",
                                  "female_age_20",
                                  "female_age_21",
                                  "female_age_22to24",
                                  "female_age_25to29",
                                  "female_age_30to34",
                                  "female_age_35to39",
                                  "female_age_40to44",
                                  "female_age_45to49",
                                  "female_age_50to54",
                                  "female_age_55to59",
                                  "female_age_60to61",
                                  "female_age_62to64",
                                  "female_age_65to66",
                                  "female_age_67to69",
                                  "female_age_70to74",
                                  "female_age_75to79",
                                  "female_age_80to84",
                                  "female_age_85plus"),
                 variable.name = "demo_info",
                 value.name = "demo_pop")
philly_cen2000_long$demo_info <- as.character(philly_cen2000_long$demo_info)
philly_cen2000_long$sex_label <- 
  ifelse(substr(philly_cen2000_long$demo_info,1, 4) %in% "male", "male",
         ifelse(substr(philly_cen2000_long$demo_info,1, 3) %in% "fem", 
                "female","unknown"))
philly_cen2000_long$age_group <- gsub(x = philly_cen2000_long$demo_info,
                                      pattern = "female|male|_|age",
                                      replacement = "")
philly_cen2000_long <- philly_cen2000_long %>%
  group_by(sex_label, age_group) %>%
  dplyr::summarize(demo_pop = sum(demo_pop, na.rm = T)) %>%
  ungroup()

###### MERGE POP DATA WITH MORTALITY DATA
philly_pop_mort_asy <- merge(philly_cen2000_long,
                             philly_mort_asy,
                             by = c("sex_label", "age_group"),
                             all = T)
philly_pop_mort_asy$age_cont <- 
  as.numeric(substr(philly_pop_mort_asy$age_group, 1, 2))
print(table(philly_pop_mort_asy$age_cont, useNA = "ifany"))

philly_pop_mort_asy$year_factor <- 
  factor(philly_pop_mort_asy$year,
         levels = c(2000, 2001, 2002, 2003, 2004, 2005, 
                    2006, 2007, 2008, 2009, 2010))

####################################################
# MODEL ATTEMPT #1: SIMPLE POISSON
# PHILLY AGGREGATED,
# DEATHS ~ AGE + SEX + YEAR
# USING CONTINUOUS AGE VARIABLE, FACTOR YEARS

philly_simple_m1 <- glm(formula = deaths ~ year_factor + male + age_cont,
                        family = poisson(link = "log"), 
                        data = philly_pop_mort_asy,
                        offset = log(demo_pop))
print(summary(philly_simple_m1))
# Call:
# glm(formula = deaths ~ year_factor + male + age_cont, 
# family = poisson(link = "log"), 
# data = philly_pop_mort_asy, offset = log(demo_pop))

# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -10.763   -3.501   -1.603    1.861   23.940  

# Coefficients:
#                  Estimate Std. Error  z value Pr(>|z|)    
# (Intercept)     -8.499242   0.013024 -652.567  < 2e-16 ***
# year_factor2001 -0.025982   0.010868   -2.391   0.0168 *  
# year_factor2002 -0.045433   0.010922   -4.160 3.19e-05 ***
# year_factor2003 -0.054193   0.010946   -4.951 7.39e-07 ***
# year_factor2004 -0.107266   0.011099   -9.665  < 2e-16 ***
# year_factor2005 -0.107851   0.011100   -9.716  < 2e-16 ***
# year_factor2006 -0.139109   0.011193  -12.429  < 2e-16 ***
# year_factor2007 -0.147518   0.011218  -13.150  < 2e-16 ***
# year_factor2008 -0.188050   0.011342  -16.580  < 2e-16 ***
# year_factor2009 -0.202006   0.011385  -17.743  < 2e-16 ***
# year_factor2010 -0.231470   0.011479  -20.165  < 2e-16 ***
# male             0.445188   0.004909   90.686  < 2e-16 ***
# age_cont         0.072327   0.000148  488.641  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

#     Null deviance: 382736  on 505  degrees of freedom
# Residual deviance:  19797  on 493  degrees of freedom
# AIC: 23179

# Number of Fisher Scoring iterations: 5

print(exp(coef(summary(philly_simple_m1))[,c("Estimate", "Std. Error")]))
#                     Estimate Std. Error
# (Intercept)     0.0002036226   1.013110
# year_factor2001 0.9743529960   1.010927
# year_factor2002 0.9555840522   1.010982
# year_factor2003 0.9472487759   1.011007
# year_factor2004 0.8982863138   1.011160
# year_factor2005 0.8977617160   1.011162
# year_factor2006 0.8701328981   1.011256
# year_factor2007 0.8628468174   1.011281
# year_factor2008 0.8285730940   1.011406
# year_factor2009 0.8170902308   1.011450
# year_factor2010 0.7933667522   1.011545
# male            1.5607834361   1.004921
# age_cont        1.0750062514   1.000148