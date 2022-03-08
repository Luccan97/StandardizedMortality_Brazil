
install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")

library(microdatasus)
library(tidyverse)
library(stringi)
library(sf)
# Creating Mortality standarized rate, UF, sex age.
# data1
# data2
# data3


# World Standard Population by Sex and Age data
standard_pop <- vroom::vroom("https://www.opendata.nhs.scot/dataset/4dd86111-7326-48c4-8763-8cc4aa190c3e/resource/2f493d21-fd39-48f9-ad6a-9b2c95b32e30/download/world_standard_population_by_sex.csv")

## Cleaning agegroup variable, changings patterns of categorical variables
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
# Dados SIM de 2010 a 2019 em todos os Estados.
dados <- fetch_datasus(year_start = 2010, year_end = 2019,
                         information_system = "SIM-DO", 
                         vars = c('CODMUNRES', 'DTOBITO','SEXO','CAUSABAS_O','IDADE'))

# Processar gera dados com Informações espaciais do municipio de residência e idade em anos.
# CID da Causa básica.
dados <- process_sim(dados)

# Tidying data 
# Create variable year
# Create variable agegroups
# Group by year, sex, agegroup, UF, CID
# Create variable with ICD chapter (first letter)

obts <- dados %>%
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
    ICD1 == "A" | ICD1 == "B" ~ "01.Algumas doenças infecciosas e parasitárias",
    ICD1 == "C" | (ICD1 == "D" & ICD2 <5) ~ "02.Neoplasias (tumores)",
    ICD1 == "D" & ICD2 >=5 ~ "03.Doenças do sangue e dos órgãos hematopoiéticos",
    ICD1 == "E" ~ "04.Doenças endócrinas nutricionais e metabólicas",
    ICD1 == "F" ~ "05.Transtornos mentais e comportamentais",
    ICD1 == "G" ~ "06.Doenças do sistema nervoso",
    ICD1 == "H" & ICD2 <6 ~"07.Doenças do olho e anexos",
    ICD1 == "H" & ICD2 >=6 ~ "08.Doenças do ouvido e da apófise mastóide",
    ICD1 == "I" ~ "09.Doenças do aparelho circulatório",
    ICD1 == "J" ~ "10.Doenças do aparelho respiratório",
    ICD1 == "K" ~ "11.Doenças do aparelho digestivo",
    ICD1 == "L" ~ "12.Doenças da pele e do tecido subcutâneo",
    ICD1 == "M" ~ "13.Doenças sistEma osteomuscular e tecido conjuntivo",
    ICD1 == "N" ~ "14.Doenças do aparelho geniturinário",
    ICD1 == "O" ~ "15.Gravidez parto e puerpério",
    ICD1 == "P" ~ "16.Algumas condições originadas no período perinatal",
    ICD1 == "Q" ~ "17.Malformações congênitas deformidades e anomalias cromossômicas",
    ICD1 == "R" ~ "18.Sintomas, sinais e achados clínicos e laboratoriais anormais, não classificados",
    ICD1 == "S"|ICD1=="T" ~ "19.Lesões envenenamento e outras causas externas",
    ICD1 == "V"|ICD1=="W"|ICD1 == "X"|ICD1 == "Y" ~ "20.Causas externas de morbidade e mortalidade",
    ICD1 == "Z" ~ "21.Fatores que influenciam o estado de saúde e o contato com os serviços de saúde",
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

# UF shapefile
UF_shp <- read_sf("data/BR_UF_2020.shp") %>%
  mutate(NM_UF = stri_trans_general(str_to_title(NM_UF), id = "Latin-ASCII")) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

# Manter apenas as bases limpas e salvar em RData.
rm(list=setdiff(ls(), c("obts_clean", "pop_t_clean", "standard_pop_clean", "UF_shp")))

write.csv(obts_clean, "obts_clean.csv",row.names = F)
write.csv(standard_pop_clean, "standard_pop_clean.csv",row.names = F)
write.csv(pop_t_clean, "pop_t_clean.csv",row.names = F)
#write_sf(UF_shp, "UF_shp.shp")
saveRDS(UF_shp, "UF_shp.RDS")