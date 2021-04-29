library(readxl)
library(dplyr)
library(readr)

municipios = read_csv("input/diretorio_municipios.csv")

AL_2018_2020 = read_excel("input/raw/AL_2018-2020.xlsx")

AL_Tratado =
AL_2018_2020 %>%
  transmute(year = lubridate::year(Data),
            month = lubridate::month(Data), 
            day = lubridate::day(Data), 
            city = stringr::str_to_lower(`Município`) %>% 
              stringi::stri_trans_general(str = .,id = "Latin-ASCII"),
            neighbour = NA,
            state = "AL",
            crime = stringr::str_to_lower(`Subjetividade Complementar`) %>% 
              stringi::stri_trans_general(str = .,id = "Latin-ASCII"), 
            sex_victim = Sexo,
            age_victim = Idade %>% parse_number(), 
            race_victim = stringr::str_to_lower(`Cor/Raça`),
            school_victim = stringr::str_to_lower(`Escolaridade`) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII"), 
            motivation = NA) %>% 
  mutate(sex_victim = gsub("Masculino","M", sex_victim),
         sex_victim = gsub("Feminino", "F", sex_victim)) %>% 
  mutate(race_victim = na_if(race_victim,"ni")) %>% 
  mutate(city = gsub("olho d agua das flores","olho d'agua das flores", city),
         city = gsub("tanque d arca", "tanque d'arca", city),
         city = gsub("olho d agua grande", 	"olho d'agua grande", city),
         city = gsub("olho d agua do casado","olho d'agua do casado", city)) %>%
  mutate(school_victim = na_if(school_victim,"ni")) %>% 
  left_join(municipios %>% mutate(municipio2 = municipio %>% stringi::stri_trans_general(str = ., 
                                                                                         id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)
           
saveRDS(AL_Tratado, "input/clean/T_AL_2018-2020.rds")

