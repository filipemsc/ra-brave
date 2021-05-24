library(readxl)
library(dplyr)
library(readr)
library(tidylog)
library(tidyverse)

municipios = read_csv("input/diretorio_municipios.csv")

DF_2018_2020 = read_excel("input/raw/DF_2018-2020.xlsx")

DF_2018_2020 <- DF_2018_2020 %>% separate(`Data Início do Fato`, c("Data", "Hora"), " ")

DF_Tratado <-   DF_2018_2020 %>% 
               transmute(id_ocorr = NA, 
                year = lubridate::year(Data), 
                month = lubridate::month(Data), 
                day = lubridate::day(Data),
                city = stringr::str_to_lower(`Cidade do Fato`) %>% 
                stringi::stri_trans_general(str = .,id = "Latin-ASCII"),
                neighbour = NA,
                state = "DF",
                crime = stringr::str_to_lower(Natureza) %>% 
                  stringi::stri_trans_general(str = .,id = "Latin-ASCII"),
                sex_victim = Sexo,
                age_victim = `Idade ocorrência` %>% parse_number(),
                race_victim = stringr::str_to_lower(`Pessoa Raça Padronizada`),
                school_victim = stringr::str_to_lower(`Grau de Instrução`) %>% 
                stringi::stri_trans_general(str = ., id = "Latin-ASCII"),
                motivation = stringr::str_to_lower(`Motivo do Crime`) %>% 
                stringi::stri_trans_general(str = ., id = "Latin-ASCII")) %>%
                mutate(sex_victim = gsub("Masculino","M", sex_victim),
                sex_victim = gsub("Feminino", "F", sex_victim)) %>% 
                mutate(race_victim = na_if(race_victim,"ni"))

DF_Tratado$id_municipio <- 5300108
DF_Tratado$id_estado <- 53


saveRDS(DF_Tratado, "input/clean/T_DF_2018-2020.rds")

