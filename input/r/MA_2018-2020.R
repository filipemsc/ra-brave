## oiooi

library(readxl)
library(dplyr)
library(readr)

municipios = read_csv("input/diretorio_municipios.csv")

MA_2018_2020 = read_excel("input/raw/MA_2018-2020.xlsx", 
                          col_types = c("numeric", "date", "date", "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", "text"))

MA_Tratado =
MA_2018_2020 %>%
  mutate(CRIME = case_when(FEMINICÍDIO == "FEMINICÍDIO" ~ "feminicidio", TRUE ~"homicidio")) %>% 
  transmute(id_ocorr = NA, 
    year = lubridate::year(DATA),
            month = lubridate::month(DATA), 
            day = lubridate::day(DATA), 
            city = stringr::str_to_lower(MUNICIPIO) %>% stringi::stri_trans_general(str = ., 
                                                                                    id = "Latin-ASCII"),
            neighbour = stringr::str_to_lower(BAIRRO) %>% stringi::stri_trans_general(str = ., 
                                                                                      id = "Latin-ASCII"),
            state = "MA",
            crime = stringr::str_to_lower(CRIME) %>% stringi::stri_trans_general(str = ., 
                                                                                 id = "Latin-ASCII"), 
            sex_victim = stringr::str_to_lower(`SEXO`),
            age_victim = parse_number(`IDADE`), 
            race_victim = NA,
            school_victim = NA, 
            motivation = NA) %>% 
  mutate(sex_victim = gsub("masculino","M", sex_victim),
         sex_victim = gsub("feminino", "F", sex_victim),
         sex_victim = na_if(sex_victim, "n/i")) %>%
  mutate(city = gsub("itapecuru-mirim","itapecuru mirim", city), 
         city = gsub("cachoeira  grande", "cachoeira grande", city), 
         city = gsub("governador edson lobao","governador edison lobao", city), 
         city = gsub("peri-mirim", "peri mirim", city), 
         city = gsub("olinda nova$", "olinda nova do maranhao", city),
         city = gsub("sao jose ribamar", "sao jose de ribamar", city),
         city = gsub("santo amaro$", "santo amaro do maranhao", city), 
         city = gsub("duque barcelar", "duque bacelar", city),
         city = gsub("nova olinda$", "nova olinda do maranhao", city),
         city = gsub("presidente jucelino", "presidente juscelino", city),
         city = gsub("central$", "central do maranhao", city), 
         city = gsub("amapa do marahao", "amapa do maranhao", city),
         city = gsub("fortaleza do nogueira", "fortaleza dos nogueiras", city),
         city = gsub("fortaleza dos nogueira$", "fortaleza dos nogueiras", city),
         city = gsub("lagoa grande$", "lagoa grande do maranhao", city),
         city = gsub("olho d´agua das cunhas", "olho d'agua das cunhas", city), 
         city = gsub("sao luis gonzaga$", "sao luis gonzaga do maranhao", city),
         city = gsub("santa filomena","santa filomena do maranhao", city),
         city = gsub("sao domingos$","sao domingos do maranhao", city),
         city = gsub("conceicao do lago acu", "conceicao do lago-acu", city), 
         city = gsub("feira nova$", "feira nova do maranhao", city)) %>% 
  left_join(municipios %>% mutate(municipio2 = municipio %>% stringi::stri_trans_general(str = ., 
                                                                                         id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

saveRDS(MA_Tratado, "input/clean/T_MA_2018-2020.rds")
