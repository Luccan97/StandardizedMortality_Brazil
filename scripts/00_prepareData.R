# 00_prepareData.R

# We will need to run this script just one time, after we need to pull to GitHub
# repository.

#remotes::install_github("rfsaldanha/microdatasus")

library(microdatasus)
library(tidyverse)
library(stringi)
library(sf)

# Standarized Mortality Rate Dashboard
# To build the standarized mortality rate for the States of Brazil
# Between the years of 2010 and 2019 we will need thre datasets
# data1 (standard_pop)
# The first dataset is the  World Standard Population by Sex and Age 
# data2 (obts)
# The second dataset is from the Mortality Sisyem Information 
# data3 (pop_t)
# The third one is the estimated populations for each State

# In this script we are organizaing, reshaping the datasets so they are matchable

# World Standard Population by Sex and Age data
standard_pop <- vroom::vroom("https://www.opendata.nhs.scot/dataset/4dd86111-7326-48c4-8763-8cc4aa190c3e/resource/2f493d21-fd39-48f9-ad6a-9b2c95b32e30/download/world_standard_population_by_sex.csv")

## Cleaning agegroup variable, renaming,changings patterns of categorical variables
standard_pop_clean <- standard_pop %>%
  mutate(
    AgeGroup = str_replace_all(AgeGroup, "years", ""),
    AgeGroup = str_replace_all(AgeGroup, "plus", ""),  
    AgeGroup = str_replace_all(AgeGroup, " ", "")) %>% 
  rename(pop = WorldStandardPopulation,
         agegroup = AgeGroup) %>%
  mutate(Sex = case_when(Sex == "Male" ~ "m",
                         Sex == "Female" ~ "f"))

# Death data.
# data from all Brazilian States between 2010 and 2019
# Here we use this function fecht_datasus from the package microdatsus
# specially made for importing Brazilian Public health data

obts <- fetch_datasus(year_start = 2010,
                      year_end = 2019,
                      # Selection of information system
                      information_system = "SIM-DO", 
                      # Selection variables of interest
                      vars = c('CODMUNRES', 'DTOBITO','SEXO','CAUSABAS_O','IDADE'))

# Geoprocessing
obts <- process_sim(obts)

# Tidying 
# Create variable year
# Create variable agegroups
# Group by year, sex, agegroup, UF, CID
# Create variable with ICD chapter (first letter)
obts <- obts %>%
  select(DTOBITO, SEXO, IDADEanos, CAUSABAS_O, munResUf) %>%
  filter(SEXO != "0") %>%
  replace(is.na(.), 0) %>%
  mutate(IDADEanos = as.numeric(IDADEanos)) %>%
  mutate(agegroup = case_when(IDADEanos < 5 ~ "0-4",
                              IDADEanos >= 5 & IDADEanos < 10 ~ "5-9",
                              IDADEanos >= 10 & IDADEanos < 15 ~ "10-14",
                              IDADEanos >= 15 & IDADEanos < 20 ~ "15-19",
                              IDADEanos >= 20 & IDADEanos < 25 ~ "20-24",
                              IDADEanos >= 25 & IDADEanos < 30 ~ "25-29",
                              IDADEanos >= 30 & IDADEanos < 35 ~ "30-34",
                              IDADEanos >= 35 & IDADEanos < 40 ~ "35-39",
                              IDADEanos >= 40 & IDADEanos < 45 ~ "40-44",
                              IDADEanos >= 45 & IDADEanos < 50 ~ "45-49",
                              IDADEanos >= 50 & IDADEanos < 55 ~ "50-54",
                              IDADEanos >= 55 & IDADEanos < 60 ~ "55-59",
                              IDADEanos >= 60 & IDADEanos < 65 ~ "60-64",
                              IDADEanos >= 65 & IDADEanos < 70 ~ "65-69",
                              IDADEanos >= 70 & IDADEanos < 75 ~ "70-74",
                              IDADEanos >= 75 & IDADEanos < 80 ~ "75-79",
                              IDADEanos >= 80 & IDADEanos < 85 ~ "80-84",
                              IDADEanos >= 85 ~ "85")) %>%
  mutate(ano = lubridate::year(DTOBITO)) %>%
  mutate(ICD1 = substr(CAUSABAS_O,1,1),
         ICD2 = as.numeric(substr(CAUSABAS_O,2,2))) 



obts_clean <- obts %>%
  mutate(ICD_chapter = case_when(
    ICD1 == "A" | ICD1 == "B" ~ "01. Certain infectious and parasitic diseases",
    ICD1 == "C" | (ICD1 == "D" & ICD2 <5) ~ "02. Neoplasms",
    ICD1 == "D" & ICD2 >=5 ~ "03. Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
    ICD1 == "E" ~ "04. Endocrine, nutritional and metabolic diseases",
    ICD1 == "F" ~ "05. Mental, Behavioral and Neurodevelopmental disorders",
    ICD1 == "G" ~ "06. Diseases of the nervous system",
    ICD1 == "H" & ICD2 <6 ~"07. Diseases of the eye and adnexa",
    ICD1 == "H" & ICD2 >=6 ~ "08. Diseases of the ear and mastoid process",
    ICD1 == "I" ~ "09. Diseases of the circulatory system",
    ICD1 == "J" ~ "10. Diseases of the respiratory system",
    ICD1 == "K" ~ "11. Diseases of the digestive system",
    ICD1 == "L" ~ "12. Diseases of the skin and subcutaneous tissue",
    ICD1 == "M" ~ "13. Diseases of the musculoskeletal system and connective tissue",
    ICD1 == "N" ~ "14. Diseases of the genitourinary system",
    ICD1 == "O" ~ "15. Pregnancy, childbirth and the puerperium",
    ICD1 == "P" ~ "16. Certain conditions originating in the perinatal period",
    ICD1 == "Q" ~ "17. Congenital malformations, deformations and chromosomal abnormalities",
    ICD1 == "R" ~ "18. Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
    ICD1 == "S"|ICD1=="T" ~ "19. Injury, poisoning and certain other consequences of external causes",
    ICD1 == "V"|ICD1=="W"|ICD1 == "X"|ICD1 == "Y" ~ "20. External causes of morbidity",
    ICD1 == "Z" ~ "21. Factors influencing health status and contact with health services",
    TRUE ~ "sem info")) %>%
  filter(ICD_chapter != "sem info") %>%
  rename(Sex = SEXO,
         UF = munResUf,
         Year = ano) %>%
  mutate(Sex = case_when(Sex == "Masculino" ~ "m",
                         Sex == "Feminino" ~ "f")) %>%
  mutate(UF = stri_trans_general(str_to_title(UF), id = "Latin-ASCII"))%>%
  group_by(Year, Sex, agegroup, UF, ICD_chapter) %>%
  summarise(Deaths = n()) %>%
  filter(ICD_chapter != "sem info")


###### Population data
library(readxl)
pop_t <- read_excel("data/ProjMunic-2010_2030.xlsx", 
                    col_types = c("numeric", "text", "text", 
                                  "skip", "skip", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "skip"),
                    col_names = c('Year', 'UF', 'Sex',
                                  "0-4", "5-9", "10-14", "15-19",
                                  "20-24", "25-29",  "30-34", "35-39",
                                  "40-44", "45-49", "50-54", "55-59",
                                  "60-64", "65-69", "70-74",
                                  "75-79", "80-84", "85-89","90+"))

pop_t_clean <- pop_t %>%
  filter(!is.na(Year)) %>%
  gather("agegroup", "pop", 4:22) %>%
  mutate(agegroup = str_replace_all(agegroup, "a", "-"),
         agegroup = str_replace_all(agegroup, " ", "")) %>%
  mutate(agegroup = case_when(agegroup == "85-89" ~ "85",
                              agegroup == "90+" ~ "85",
                              TRUE ~ agegroup)) %>%
  group_by(Year,Sex,agegroup, UF) %>%
  summarise(pop = sum(pop)) %>%
  mutate(UF = stri_trans_general(str_to_title(UF), id = "Latin-ASCII"))

# UF shapefile for the interative map

UF_shp <- read_sf("data/BR_UF_2020.shp") %>%
  mutate(NM_UF = stri_trans_general(str_to_title(NM_UF), id = "Latin-ASCII")) %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  sf::st_simplify(UF_shp, dTolerance = 20000)

# Keep the clean data and save them in csv format. 
# Except UF_shp, this one we save in RDS format.
rm(list=setdiff(ls(), c("obts_clean", "pop_t_clean", "standard_pop_clean", "UF_shp")))

write.csv(obts_clean, "data/obts_clean.csv",row.names = F)
write.csv(standard_pop_clean, "data/standard_pop_clean.csv",row.names = F)
write.csv(pop_t_clean, "data/pop_t_clean.csv",row.names = F)
#write_sf(UF_shp, "UF_shp.shp")
saveRDS(UF_shp, "UF_shp.RDS")
