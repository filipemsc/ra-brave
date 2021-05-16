rm(list=ls())

library(plyr)
library(dplyr)

###### Abre os dados

diret = "input/clean"
files <- list.files(path = diret, pattern = '\\.rds', full.names = TRUE)
tables <- lapply(files, readRDS)
dados_br <- do.call(rbind.fill, tables)

###### Formatação

dados_br <- dados_br %>%
  mutate(date = as.POSIXct(paste(year, month, day, sep = "-"),
                           format = '%Y-%m-%d'))

dados_br <- dados_br[order(dados_br$id_estado, dados_br$id_municipio,
                           dados_br$date), ]

saveRDS(dados_br, "input/clean/T_BR_2018-2020.rds")
