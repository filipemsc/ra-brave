library(readxl)
library(dplyr)
library(readr)

SP_feminicidios = read_excel("input/raw/SP_2017-2020.xlsx") %>% 
  filter(STATUS == "Consumado")%>%
  filter(RUBRICA != "Suicídio consumado") %>% 
  filter(!(TIPOPESSOA == "Vítima" & SEXO == "Masculino")) %>%
  filter(ANO_BO >2017) %>% 
  filter(TIPOPESSOA == "Vítima") %>%
  distinct(ANO_BO, NUM_BO, DELEGACIA_NOME, DATAOCORRENCIA, .keep_all = TRUE)

municipios = read_csv("input/diretorio_municipios.csv")

SP_feminicidios %>% 
  transmute(id_ocorr = NUMERO_BOLETIM,
            city = stringr::str_to_lower(CIDADE) %>% stringi::stri_trans_general(str = ., 
                                                                                    id = "Latin-ASCII"),
            year = lubridate::year(DATAOCORRENCIA),
            month = lubridate::month(DATAOCORRENCIA),
            day = lubridate::day(DATAOCORRENCIA),
            neighbour = stringr::str_to_lower(BAIRRO),
            state = "SP",
            crime = "feminicidio", 
            sex_victim = "F",
            age_victim = IDADE, 
            race_victim = stringr::str_to_lower(CORCUTIS),
            school_victim = GRAUINSTRUCAO, 
            motivation = NA) %>%
  mutate(city = gsub("s\\.", "sao ",city ),
         city = gsub("sao andre", "santo andre", city),
         city = gsub("sao anastacio", "santo anastacio", city), 
         city = gsub("sao isabel", "santa isabel", city),
         city = gsub("sao gertrudes", "santa gertrudes", city),
         city = gsub("mogi mirim", "moji mirim", city), 
         city = gsub("pirapora bom jesus", "pirapora do bom jesus", city),
         city = gsub("sao barbara d oeste", "santa barbara d'oeste", city),
         city = gsub("sao branca", "santa branca", city), 
         city = gsub("sao rita passa quatro", "santa rita do passa quatro", city))%>%
  left_join(municipios %>% mutate(municipio2 = stringi::stri_trans_general(str = municipio, 
                                                                           id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% View()


