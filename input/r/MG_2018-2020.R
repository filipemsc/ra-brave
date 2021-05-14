library(readxl)
library(dplyr)
library(readr)



MG_2018_2020 = readxl::read_excel("input/raw/MG_2018-2020.xlsx")
municipios = read_csv("input/diretorio_municipios.csv")

MG_Tratado = MG_2018_2020 %>%
  
  transmute(id_ocorr = NA,
    city = stringr::str_to_lower(MUNICIPIO) %>% stringi::stri_trans_general(str = ., 
                                                                                    id = "Latin-ASCII"),
            
            
            year = ANO,
            month = MÊS,
            day = DIA,
            neighbour = stringr::str_to_lower(SUBLOCALIZACAO),
            state = "MG",
            crime = stringr::str_to_lower(DELITO), 
            sex_victim = SEXO,
            age_victim = IDADE, 
            race_victim = stringr::str_to_lower(CUTIS),
            school_victim = NA, 
            motivation = MOTIVACAO) %>%
  
  mutate(city = gsub("abre-campo", "abre campo",city),
         city = gsub("pingo d'agua", "pingo-d'agua",city),
         city = gsub("brasopolis", "brazopolis",city), 
         city = gsub("entre-rios de minas", "entre rios de minas",city), 
         city = gsub("grao-mogol", "grao mogol",city), 
         city = gsub("sao sebast. do maranhao", "sao sebastiao do maranhao",city), 
         city = gsub("sao sebastiao paraiso", "sao sebastiao do paraiso",city), 
         city = gsub("estrela-dalva", "estrela dalva",city),
         city = gsub("capitao eneias", "capitao eneas",city),
         city = gsub("passa vinte", "passa-vinte",city),
         city = gsub("nao informado", NA ,city),
         city = gsub("dores de ganhaes", "dores de guanhaes",city))%>%
  
  left_join(municipios %>% mutate(municipio2 = stringi::stri_trans_general(str = municipio, 
                                                                           id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2"))%>%
  
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)



saveRDS(MG_Tratado, "input/clean/T_MG_2018-2020.rds")


