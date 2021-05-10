library(readxl)
library(dplyr)
library(readr)

# MUDEM O CAMINHO DO DESKTOP!

MG_2018_2020 = readxl::read_excel("C:/Users/pedro/Desktop/Pedro Feijó/FEA-USP/2021/BWE - EConomistas/BRAVE/Violencia Domestica/Bases_NT/MG_2018-2020.xlsx")
municipios = read_csv("C:/Users/pedro/Desktop/Pedro Feijó/FEA-USP/2021/BWE - EConomistas/BRAVE/Violencia Domestica/diretorio_municipios.csv")

MG_Tratado = MG_2018_2020 %>%
  
  transmute(city = stringr::str_to_lower(MUNICIPIO) %>% stringi::stri_trans_general(str = ., 
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
  
  left_join(municipios %>% mutate(municipio2 = stringi::stri_trans_general(str = municipio, 
                                                                           id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2"))


saveRDS(MG_Tratado, "C:/Users/pedro/Desktop/Pedro Feijó/FEA-USP/2021/BWE - Economistas/BRAVE/Violencia Domestica/Bases_T/T_MG_2018-2020.rds")
# MUDEM O CAMINHO DO DESKTOP!

