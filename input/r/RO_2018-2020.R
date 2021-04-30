library(readxl)
library(dplyr)
library(readr)

get_RO = function(){
  RO_2018 <- read_excel("input/raw/RO_2019-2020.xlsx", 
                        sheet = "ESTADO RO 2018")
  RO_2018[] <- lapply(RO_2018, as.character)
  
  RO_2019 <- read_excel("input/raw/RO_2019-2020.xlsx",
                      sheet = "ESTADO RO 2019")
  RO_2019[] <- lapply(RO_2019, as.character)
  
  RO_2020 <- read_excel("input/raw/RO_2019-2020.xlsx", 
                                      sheet = "ESTADO RO 2020")
  RO_2020[] <- lapply(RO_2020, as.character)
  
  rbind(RO_2018,RO_2019,RO_2020)
}

RO_2018_2020 = get_RO()

municipios = read_csv("input/diretorio_municipios.csv")

repx = RO_2018_2020[,"TOTAL"] %>% purrr::as_vector()

idx = rep(1:nrow(RO_2018_2020), repx)

RO_Tratado = RO_2018_2020[idx,] %>% 
  transmute(year = readr::parse_number(ano),
            month = readr::parse_number(mes), 
            day = readr::parse_number(dia), 
            city = stringr::str_to_lower(municipio_fato) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII"),
            neighbour = stringr::str_to_lower(bairro_fato) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII"),
            state = "RO",
            crime = stringr::str_to_lower(natureza_fato) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII") , 
            crime = case_when(crime == "feminicidio" ~ "feminicidio", TRUE ~"homicidio"),
            sex_victim = gsub("MASCULINO", "M", SEXO),
            sex_victim = gsub("FEMININA", "F", sex_victim),
            sex_victim = gsub("FEMININO", "F", sex_victim), 
            sex_victim = gsub(" ","", sex_victim),
            sex_victim = gsub("^M,M,M$", "M", sex_victim),
            sex_victim = gsub("^M,M$", "M", sex_victim),
            sex_victim = gsub("^F,F$", "F", sex_victim),
            age_victim = NA, 
            race_victim = NA,
            school_victim = NA, 
            motivation = NA,
            id_ocorr = nr_ocorr) %>%
  mutate(city = gsub("espigao do oeste", "espigao d'oeste", city)) %>%
  left_join(municipios %>% mutate(municipio2 = municipio %>% stringi::stri_trans_general(str = ., 
                                                                                         id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>%
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)
  
RO_Tratado %>% count(sex_victim)  
  
RO_Tratado[RO_Tratado$sex_victim == "F,M,F","sex_victim"] <- rep(c("F","M","F"), 
                                                          nrow(RO_Tratado[RO_Tratado$sex_victim == "F,M,F",])/3)

RO_Tratado[RO_Tratado$sex_victim == "M,M,NÃOINFORMADO","sex_victim"] <- rep(c("M","M", "NÃOINFORMADO"), 
                                                                 nrow(RO_Tratado[RO_Tratado$sex_victim == "M,M,NÃOINFORMADO",])/3)

RO_Tratado[RO_Tratado$sex_victim == "M,M,F","sex_victim"] <- rep(c("M","M","F"), 
                                                                 nrow(RO_Tratado[RO_Tratado$sex_victim == "M,M,F",])/3)

RO_Tratado[RO_Tratado$sex_victim == "M,F","sex_victim"] <- rep(c("M","F"), 
                                                                 nrow(RO_Tratado[RO_Tratado$sex_victim == "M,F",])/2)

RO_Tratado[RO_Tratado$sex_victim == "F,M","sex_victim"] <- rep(c("F","M"), 
                                                               nrow(RO_Tratado[RO_Tratado$sex_victim == "F,M",])/2)

RO_Tratado[RO_Tratado$sex_victim == "NÃOINFORMADO","sex_victim"] <- rep(NA, 
                                                                        nrow(RO_Tratado[RO_Tratado$sex_victim == "NÃOINFORMADO",]))

saveRDS(RO_Tratado, "input/clean/T_RO_2018-2020.rds")
