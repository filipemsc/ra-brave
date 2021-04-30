library(readxl)
library(dplyr)
library(readr)

municipios = read_csv("input/diretorio_municipios.csv")

MT_2018 = read_excel("input/raw/MT_2018-2020.xlsx", sheet = "2018")
MT_2019 = read_excel("input/raw/MT_2018-2020.xlsx", sheet = "2019")
MT_2020 = read_excel("input/raw/MT_2018-2020.xlsx", sheet = "2020")

MT_1 = MT_2018 %>% 
  transmute(id_ocorr = NA,
    year = lubridate::year(`DATA DO FATO`),
            month = lubridate::month(`DATA DO FATO`), 
            day = lubridate::day(`DATA DO FATO`), 
            city = stringr::str_to_lower(`MUNICÍPIO`) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII") ,
            neighbour = NA,
            state = "MT",
            crime = stringr::str_to_lower(`NATUREZA DA OCORRÊNCIA`) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII") , 
            sex_victim = `SEXO`,
            age_victim = `IDADE`, 
            race_victim = stringr::str_to_lower(RAÇA),
            school_victim = ESCOLARIDADE, 
            motivation = stringr::str_to_lower(MOTIVAÇÃO) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII")) %>% 
  mutate(sex_victim = gsub("MASCULINO", "M", sex_victim),
         sex_victim = gsub("FEMININO", "F", sex_victim),
         sex_victim = na_if(sex_victim, "NAO IDENTIFICADO")) %>% 
  mutate(city = gsub("nova bandeirante", "nova bandeirantes", city),
         city = gsub("vila bela da ss trindade", "vila bela da santissima trindade", city), 
         city = gsub("nossa sra do livramento","nossa senhora do livramento",city),
         city = gsub("nova monteverde", "nova monte verde", city),
         city = gsub("gloria doeste","gloria d'oeste",city),
         city = gsub("mirassol d\"oeste", "mirassol d'oeste", city),
         city = gsub("santa carmen", "santa carmem", city),
         city = gsub("figueiropolis d\"oeste", "figueiropolis d'oeste", city),
         city = gsub("poxoreu", "poxoreo", city)) %>%
  left_join(municipios %>% mutate(municipio2 = municipio %>% 
                                    stringi::stri_trans_general(str = ., id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

MT_2 = MT_2019 %>% 
  transmute(id_ocorr = NA,
            year = lubridate::year(`DATA DO FATO`),
            month = lubridate::month(`DATA DO FATO`), 
            day = lubridate::day(`DATA DO FATO`), 
            city = stringr::str_to_lower(`MUNICÍPIO`) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII") ,
            neighbour = NA,
            state = "MT",
            crime = stringr::str_to_lower(`NATUREZA DA OCORRÊNCIA`) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII") , 
            sex_victim = `SEXO`,
            age_victim = `IDADE`, 
            race_victim = stringr::str_to_lower(RAÇA),
            school_victim = ESCOLARIDADE, 
            motivation = stringr::str_to_lower(MOTIVAÇÃO) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII")) %>% 
  mutate(sex_victim = gsub("MASCULINO", "M", sex_victim),
         sex_victim = gsub("FEMININO", "F", sex_victim),
         sex_victim = na_if(sex_victim, "NAO IDENTIFICADO")) %>% 
  mutate(city = gsub("nova bandeirante", "nova bandeirantes", city),
         city = gsub("vila bela da ss trindade", "vila bela da santissima trindade", city), 
         city = gsub("nossa sra do livramento","nossa senhora do livramento",city),
         city = gsub("nova monteverde", "nova monte verde", city),
         city = gsub("gloria doeste","gloria d'oeste",city),
         city = gsub("mirassol d\"oeste", "mirassol d'oeste", city),
         city = gsub("santa carmen", "santa carmem", city),
         city = gsub("figueiropolis d\"oeste", "figueiropolis d'oeste", city),
         city = gsub("poxoreu", "poxoreo", city)) %>%
  left_join(municipios %>% mutate(municipio2 = municipio %>% 
                                    stringi::stri_trans_general(str = ., id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

MT_3 = MT_2020 %>% 
  transmute(id_ocorr = NA,
            year = lubridate::year(`DATA DO FATO`),
            month = lubridate::month(`DATA DO FATO`), 
            day = lubridate::day(`DATA DO FATO`), 
            city = stringr::str_to_lower(`MUNICÍPIO`) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII") ,
            neighbour = NA,
            state = "MT",
            crime = stringr::str_to_lower(`NATUREZA DA OCORRÊNCIA`) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII") , 
            sex_victim = `SEXO`,
            age_victim = `IDADE`, 
            race_victim = stringr::str_to_lower(RAÇA),
            school_victim = ESCOLARIDADE, 
            motivation = stringr::str_to_lower(MOTIVAÇÃO) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII")) %>% 
  mutate(sex_victim = gsub("MASCULINO", "M", sex_victim),
         sex_victim = gsub("FEMININO", "F", sex_victim),
         sex_victim = na_if(sex_victim, "NAO IDENTIFICADO")) %>% 
  mutate(city = gsub("nova bandeirante", "nova bandeirantes", city),
         city = gsub("vila bela da ss trindade", "vila bela da santissima trindade", city), 
         city = gsub("nossa sra do livramento","nossa senhora do livramento",city),
         city = gsub("nova monteverde", "nova monte verde", city),
         city = gsub("gloria doeste","gloria d'oeste",city),
         city = gsub("mirassol d\"oeste", "mirassol d'oeste", city),
         city = gsub("santa carmen", "santa carmem", city),
         city = gsub("figueiropolis d\"oeste", "figueiropolis d'oeste", city),
         city = gsub("poxoreu", "poxoreo", city),
         city = gsub("altos da boa vista", "alto boa vista", city)) %>%
  left_join(municipios %>% mutate(municipio2 = municipio %>% 
                                    stringi::stri_trans_general(str = ., id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)


MT_Tratado = rbind(MT_1, MT_2, MT_3) %>%
  mutate(race_victim = na_if(race_victim, "ni"),
         race_victim = na_if(race_victim, "não"),
         race_victim = na_if(race_victim, "nao informado"),
         race_victim = na_if(race_victim, "n"),
         race_victim = gsub("branco", "branca", race_victim)) %>%
  mutate(sex_victim = na_if(sex_victim,"NÃO INFORMADO")) %>%
  mutate(crime = gsub("homicidio doloso", "homicidio", crime)) %>%
  mutate(age_victim = readr::parse_number(age_victim)) %>%
  mutate(school_victim = school_victim %>% stringr::str_to_lower() %>%
           stringi::stri_trans_general(str = ., id = "Latin-ASCII"),
         school_victim = na_if(school_victim, "nao informado"),
         school_victim = na_if(school_victim, "ni"))

saveRDS(MT_Tratado, "input/clean/T_MT_2018-2020.rds")
