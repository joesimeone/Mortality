rm(list=ls())

library("tidyverse")
library("readxl")
library("janitor")
library("writexl")


##A function that imports separate mortality years as separate objects. BAD
import_vs <- function(filename, skip_length, sheetname){read_excel(filename, skip = skip_length,
                                                                   sheet = sheetname)}

vs_2014_m <- import_vs(filename = "DE_Vital_Stats_Mort_2014.xlsx", skip_length=8, sheetname = 'NDRCAGML') 
vs_2014_f <- import_vs(filename = "DE_Vital_Stats_Mort_2014.xlsx", skip_length=8, sheetname = 'NDRCAGFE')

vs_2015_m <- import_vs(filename = "DE_Vital_Stats_Mort_2015.xlsx", skip_length=5, sheetname = 'NDRCAGML')
vs_2015_f <- import_vs(filename = "DE_Vital_Stats_Mort_2015.xlsx", skip_length=5, sheetname = 'NDRCAGFE')

vs_2016_m <- import_vs(filename = "DE_Vital_Stats_Mort_2016.xlsx", skip_length=4, sheetname = 'NDRCAGML')
vs_2016_f <- import_vs(filename = "DE_Vital_Stats_Mort_2016.xlsx", skip_length=5, sheetname = 'NDRCAGFE')

vs_2017_m <- import_vs(filename = "DE_Vital_Stats_Mort_2017.xlsx", skip_length=4, sheetname = 'TABLE F-4')
vs_2017_f <- import_vs(filename = "DE_Vital_Stats_Mort_2017.xlsx", skip_length=5, sheetname = 'TABLE F-5')

vs_2018_m <- import_vs(filename = "DE_Vital_Stats_Mort_2018.xlsx", skip_length=5, sheetname = 'TABLE F-4')
vs_2018_f <- import_vs(filename = "DE_Vital_Stats_Mort_2018.xlsx", skip_length=5,  sheetname = 'TABLE F-5')

vs_2019_m <- import_vs(filename = "DE_Vital_Stats_Mort_2019.xlsx", skip_length=5, sheetname = 'TABLE F-4')
vs_2019_f <- import_vs(filename = "DE_Vital_Stats_Mort_2019.xlsx", skip_length=5, sheetname = 'TABLE F-5')

vs_2020_m <- import_vs(filename = "DE_Vital_Stats_Mort_2020.xlsx", skip_length=5, sheetname = 'TABLE F-4')
vs_2020_f <- import_vs(filename = "DE_Vital_Stats_Mort_2020.xlsx", skip_length=5, sheetname = 'TABLE F-5')

## Function for 2014 - 2016 data - Have to do something different for 17 - 20 because of new hispanic variable
tidy_vitalstats_14to16 <- function(dat, year, sex_dum){dat %>% clean_names() %>% select(!c('total')) %>% rename('xunk' = 'unk') %>% 
    
    pivot_longer(cols = starts_with('x'), names_to = 'age_group_rough',
                 values_to = 'death_count') %>% 
    
    mutate(year := {{ year }},
           sex := {{ sex_dum }},
           age_group = str_remove(age_group_rough, "x"),
           age_group = str_replace(age_group, "_", "-"),
           age_group = str_replace(age_group, "75", "75+"),
           
           
           county = case_when( race == "New Castle" ~ "ncc",
                               race == "Kent" ~ "k",
                               race == "Sussex" ~ "sus",
                               race == "Wilmington" ~ "wilm_drop",
                               race == "NC County" ~ 'ncc_drop',
                               race == "Delaware" ~ "de_drop"
           ),
           
           race_fix = case_when(race == "White" ~ "w",
                                race == "Black" ~ "b",
                                race == "Other" ~ "o"),
           
           age_group_dummy = case_when(age_group == "1" ~ 0,
                                       age_group == "1-4" ~ 0,
                                       age_group == "5-9" ~ 1,
                                       age_group == "10-14" ~ 2,
                                       age_group == "15-19" ~ 3,
                                       age_group == "20-24" ~ 4,
                                       age_group == "25-34" ~ 5,
                                       age_group == "35-44" ~ 6,
                                       age_group == "45-54" ~ 7,
                                       age_group == "55-64" ~ 8,
                                       age_group == "65-74" ~ 9,
                                       age_group == "75+" ~ 10,
                                       age_group == "Unk" ~ 999,
                                       TRUE ~ 1000)) %>%
    
        fill(county) %>% na.omit %>% select(!c('age_group_rough', 'race')) %>% rename('race' = 'race_fix')}

#Function call for 14 to 16, 1st step of cleaning
F_2014_raw <- tidy_vitalstats_14to16(dat=vs_2014_f, year=2014, sex_dum="F")
M_2014_raw <- tidy_vitalstats_14to16(dat=vs_2014_m, year=2014, sex_dum="M")

F_2015_raw <- tidy_vitalstats_14to16(dat=vs_2015_f, year=2015, sex_dum="F")
M_2015_raw <- tidy_vitalstats_14to16(dat=vs_2015_m, year=2015, sex_dum="M") 

F_2016_raw <- tidy_vitalstats_14to16(dat=vs_2016_f, year=2016, sex_dum="F")
M_2016_raw <- tidy_vitalstats_14to16(dat=vs_2016_f, year=2016, sex_dum="M") 

##A function to get rid of variables that will mess up any merges with other ungrouped death data
drop_some_vars <- function(dat){dat <- dat[!(dat$county == "de_drop" | dat$county == "ncc_drop"
                                             | dat$county == "wilm_drop"),]}

##Function call to remove extraneous variables
F_2014_clean <- drop_some_vars(dat=F_2014_raw)
M_2014_clean <- drop_some_vars(dat=M_2014_raw)

F_2015_clean <- drop_some_vars(dat=F_2015_raw)
M_2015_clean <- drop_some_vars(dat=M_2015_raw)

F_2016_clean <- drop_some_vars(dat=F_2016_raw)
M_2016_clean <- drop_some_vars(dat=M_2016_raw)


##Same as other tidy_vitalstats function, except that it makes some changes to included hispanic variables
tidy_vitalstats_17to20 <- function(dat, year, sex_dum){dat %>% clean_names() %>% select(!c('total')) %>% rename('xunk' = 'unk') %>% 
    
    pivot_longer(cols = starts_with('x'), names_to = 'age_group_rough',
                 values_to = 'death_count') %>% 
    
    mutate(year := {{ year }},
           sex := {{ sex_dum }},
           age_group = str_remove(age_group_rough, "x"),
           age_group = str_replace(age_group, "_", "-"),
           age_group = str_replace(age_group, "75", "75+"),

           
           race = str_replace(race, "Non-Hispanic", "NH"),
           
           
           county = case_when( race == "New Castle" ~ "ncc",
                               race == "Kent" ~ "k",
                               race == "Sussex" ~ "sus",
                               race == "Wilmington" ~ "wilm_drop",
                               race == "NC County" ~ 'ncc_drop',
                               race == "Delaware" ~ "de_drop"
           ),
           
           race_fix = case_when(race == "NH White" ~ "w",
                                race == "NH Black" ~ "b",
                                race == "NH Other" ~ "o",
                                race == "Hispanic" ~ "h"),
           
           age_group_dummy = case_when(age_group == "1" ~ 0,
                                       age_group == "1-4" ~ 0,
                                       age_group == "5-9" ~ 1,
                                       age_group == "10-14" ~ 2,
                                       age_group == "15-19" ~ 3,
                                       age_group == "20-24" ~ 4,
                                       age_group == "25-34" ~ 5,
                                       age_group == "35-44" ~ 6,
                                       age_group == "45-54" ~ 7,
                                       age_group == "55-64" ~ 8,
                                       age_group == "65-74" ~ 9,
                                       age_group == "75+" ~ 10,
                                       age_group == "unk" ~ 999,
                                       TRUE ~ 1000)) %>%
    
    
    fill(county) %>% na.omit %>% select(!c('age_group_rough', 'race')) %>% rename('race' = 'race_fix')}


F_2017_raw <- tidy_vitalstats_17to20(dat=vs_2017_f, year=2017, sex_dum="F")
M_2017_raw <- tidy_vitalstats_17to20(dat=vs_2017_m, year=2017, sex_dum="M")

F_2018_raw <- tidy_vitalstats_17to20(dat=vs_2018_f, year=2018, sex_dum="F")
M_2018_raw <- tidy_vitalstats_17to20(dat=vs_2018_m, year=2018, sex_dum="M") 

F_2019_raw <- tidy_vitalstats_17to20(dat=vs_2019_f, year=2019, sex_dum="F")
M_2019_raw <- tidy_vitalstats_17to20(dat=vs_2019_m, year=2019, sex_dum="M") 

F_2020_raw <- tidy_vitalstats_17to20(dat=vs_2020_f, year=2020, sex_dum="F")
M_2020_raw <- tidy_vitalstats_17to20(dat=vs_2020_m, year=2020, sex_dum="M") 


##Same function calls to remove extraneuous variables 
F_2017_clean <- drop_some_vars(dat=F_2017_raw)
M_2017_clean <- drop_some_vars(dat=M_2017_raw)

F_2018_clean <- drop_some_vars(dat=F_2018_raw)
M_2018_clean <- drop_some_vars(dat=M_2018_raw)

F_2019_clean <- drop_some_vars(dat=F_2019_raw)
M_2019_clean <- drop_some_vars(dat=M_2019_raw)

F_2020_clean <- drop_some_vars(dat=F_2020_raw)
M_2020_clean <- drop_some_vars(dat=M_2020_raw)

##Gets you to a 2014-16 dataset reduced to its lowest part: county, age, sex
de_mort_fin_early <- rbind(F_2014_clean, F_2015_clean, F_2016_clean,
                  M_2014_clean, M_2015_clean, M_2016_clean)

##Gets you to a 2017-20 dataset reduced to its lowest part: county, age, sex
de_mort_fin <- rbind(F_2017_clean, F_2018_clean, F_2019_clean, F_2020_clean,
                     M_2017_clean, M_2018_clean, M_2019_clean, M_2020_clean)


writexl::write_xlsx(de_mort_fin_early, "/Users/joesimeone/Desktop/R Master/Mortality/clean_data/de_deaths_14to16.xlsx")
writexl::write_xlsx(de_mort_fin, "/Users/joesimeone/Desktop/R Master/Mortality/clean_data/de_deaths_17to20.xlsx")



##Below is to make sure that finished data is summarizing correctly
#check1 <- de_mort_fin %>% group_by(year, sex) %>% summarise(sum_dc = sum(death_count))

#check2 <- de_mort_fin %>% group_by(year, race_fix) %>% summarise(sum= sum(death_count))

#check3 <- de_mort_fin %>% group_by(year, county, age_group) %>% summarise(sum= sum(death_count))