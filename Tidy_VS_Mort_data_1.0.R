rm(list=ls())

library("tidyverse")
library("readxl")
library("janitor")

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

vs_2018_m <- import_vs(filename = "DE_Vital_Stats_Mort_2018.xlsx", sheetname = 'TABLE F-4')
vs_2018_f <- import_vs(filename = "DE_Vital_Stats_Mort_2018.xlsx", sheetname = 'TABLE F-5')

vs_2019_m <- import_vs(filename = "DE_Vital_Stats_Mort_2019.xlsx", sheetname = 'TABLE F-4')
vs_2019_f <- import_vs(filename = "DE_Vital_Stats_Mort_2019.xlsx", sheetname = 'TABLE F-5')

vs_2020_m <- import_vs(filename = "DE_Vital_Stats_Mort_2020.xlsx", skip_length=5, sheetname = 'TABLE F-4')
vs_2020_f <- import_vs(filename = "DE_Vital_Stats_Mort_2020.xlsx", skip_length=5, sheetname = 'TABLE F-5')

clean_vsf_14_1<- vs_2017_m %>% clean_names() %>% select(!c('total', 'unk')) %>% 
  
  pivot_longer(cols = starts_with('x'), names_to = 'age_group_rough',
                                                   values_to = 'death_count') %>% 
                                      
                                    mutate(year = 2014,
                                             sex = "F",
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
                                             
                                             race_fix = case_when(race == 'Non-Hispanic White' ~ "w",
                                                                  race == 'Non-Hispanic Black' ~ "b",
                                                                  race == 'Non-Hispanic Other' ~ "o"),
                                        
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
                                                                    TRUE ~ 999)) %>%
                                                                    
                                   fill(county) %>% na.omit %>% select(!c('age_group_rough', 'race'))
  
  
clean_vsf_14_2 <- clean_vsf_14_1[!(clean_vsf_14_1$county == "de_drop" | clean_vsf_14_1$county == "ncc_drop"
                             | clean_vsf_14_1$county == "wilm_drop"),]
 