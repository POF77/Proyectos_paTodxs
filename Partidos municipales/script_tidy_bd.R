# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Título: Transiciones a nivel municipal
# Fecha inicio: 16/11/20
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Datos: Sistema Nacional de Información Municipal: http://www.snim.rami.gob.mx/

# 0. Configuración

library(tidyverse)
library(xlsx)
library(stringr)

ent <- "C:/Users/Usuario/OneDrive - El Colegio de México A.C/1. COLMEX/2. Ciencia de la organización/1. Proyecto/Análisis datos/Datos completos/SNIM_2010/President_Mun_Hist/1. Entradas/"
sal <- "C:/Users/Usuario/OneDrive - El Colegio de México A.C/1. COLMEX/2. Ciencia de la organización/1. Proyecto/Análisis datos/Datos completos/SNIM_2010/President_Mun_Hist/2. Salidas/"

# Bases de datos
catalogo_geo0 <- read.csv("C:/Users/Usuario/OneDrive - El Colegio de México A.C/1. COLMEX/2. Ciencia de la organización/1. Proyecto/Análisis datos/1. Datos/Catalogo_locs_2020.11.10.csv",
                          fileEncoding = "UTF-8")

# Cargar exceles
#xls a xlsx -> https://onlineconvertfree.com/convert-format/xls-to-xlsx/
# i <- 1
# while (i <= 32){
#   muni_ <- read.xlsx2(paste0(ent, "presidentes_municipales_historico_", i, ".xlsx"),
#                       sheetIndex = 1, startRow = 4)
#   assign(paste0("muni_", i), muni_)
#   i = i + 1
# }
# 
# bd00 <- bind_rows(muni_1, muni_2, muni_3, muni_4, muni_5, muni_6, muni_7, muni_8,
#                   muni_9, muni_10, muni_11, muni_12, muni_13, muni_14, muni_15,
#                   muni_16, muni_17, muni_18, muni_19, muni_20, muni_21, muni_22,
#                   muni_23, muni_24, muni_25, muni_26, muni_27, muni_28, muni_29,
#                   muni_30, muni_31, muni_32)

#write.xlsx2(bd00, paste0(sal, "president_muni_hist_0.xlsx"),
#            sheetName = "bd", col.names = T, row.names = F)

bd0 <- read.xlsx2(paste0(sal, "president_muni_hist_0.xlsx"),
           sheetIndex = 1)

# write.csv(bd00, paste0(wd, "president_muni_hist_0.csv"))
# 
# bd0 <- read.csv("C:/Users/Usuario/OneDrive - El Colegio de México A.C/1. COLMEX/2. Ciencia de la organización/1. Proyecto/Análisis datos/Datos completos/SNIM_2010/President_Mun_Hist/president_muni_hist_0.csv",
#                 fileEncoding ="UTF-8")

# rm(muni_, muni_1, muni_2, muni_3, muni_4, muni_5, muni_6, muni_7, muni_8,
#    muni_9, muni_10, muni_11, muni_12, muni_13, muni_14, muni_15, muni_16,
#    muni_17, muni_18, muni_19, muni_20, muni_21, muni_22, muni_23, muni_24,
#    muni_25, muni_26, muni_27, muni_28, muni_29, muni_30, muni_31, muni_32)

# 1. Tidy: Catálogo localidades ------------------------------------------------

# Crear id por municipio único en el Catálogo.
catalogo_geo <- catalogo_geo0 %>%
  select(Cve_Ent, Nom_Ent, Cve_Mun, Nom_Mun)

catalogo_geo <- catalogo_geo %>%
  mutate(Cve_Mun = as.character(Cve_Mun),
         Cve_Ent = as.character(Cve_Ent))

catalogo_geo <- catalogo_geo %>%
  mutate(cve_ent = case_when(
    nchar(catalogo_geo$Cve_Ent) == 1 ~ paste0("0", catalogo_geo$Cve_Ent),
    nchar(catalogo_geo$Cve_Ent) == 2 ~ catalogo_geo$Cve_Ent))

catalogo_geo <- catalogo_geo %>%
  mutate(cve_mun = case_when(
    nchar(catalogo_geo$Cve_Mun) == 1 ~ paste0("00", catalogo_geo$Cve_Mun),
    nchar(catalogo_geo$Cve_Mun) == 2 ~ paste0("0", catalogo_geo$Cve_Mun),
    nchar(catalogo_geo$Cve_Mun) == 3 ~ Cve_Mun))

catalogo_geo <- catalogo_geo %>%
  mutate(id = paste0(cve_ent, cve_mun))

catalogo_geo <- unique(catalogo_geo)

catalogo_geo <- catalogo_geo %>%
  mutate(muni_uni = paste0(Nom_Ent, Nom_Mun))

# Tidy: bd ---------------------------------------------------------------------
bd0 <- read.xlsx2(bd0, sheetIndex = 1, startRow = 1)
glimpse(bd0)
bd <- bd0

# Integrar "id" de municipios
bd <- bd %>%
  mutate(muni_uni = paste0(Estado, Municipio))

bd <- left_join(bd, catalogo_geo, by = "muni_uni")

bd <- bd %>% select(id, cve_ent, Nom_Ent, cve_mun, Nom_Mun, Partido, Periodo)

# Filtar solo observaciones con partido político
bd <- bd %>%
  filter(nchar(Partido) > 0)

# Crear variable: fecha_inicio y fecha_fin a partir de "periodo"
bd1 <- bd %>%
  mutate(anos = str_extract_all(bd$Periodo, "\\d{4}")) %>%
  mutate(ano_ini = as.numeric(substr(anos, start = 4, stop = 7)),
         ano_fin = as.numeric(substr(anos, start = 12, stop = 15))) %>%
  select(-Periodo, -anos) %>%
  filter(ano_ini >= 1993, ano_fin <= 2011) #Transiciones justo antes del fin de Salinas.

# BD: Partidos x municipio -----------------------------------------------------
# Crear bd con municipio + número de alternancias entre 1991 y 2011
id <- unique(bd1$id)

data_partidos_muni <- tibble(id = "x", partidos = as.integer(0))
for (i in id) {
  
  list <- bd1 %>%
    filter(id == i) %>%
    distinct(Partido) %>%
    mutate(id = i)
  
  data_partidos_muni <- bind_rows(data_partidos_muni,
                                  tibble(id = i, partidos = nrow(list)))
}

data_partidos_muni <- data_partidos_muni %>%
  filter(id != "x", partidos != 0)

write.csv(data_partidos_muni, paste0(sal, "partidos_muni_hist.csv"),
          fileEncoding = "UTF-8")
# Descripción del archivo: Número de partidos político que han gobernado cada
#municipio de México entre 1993 y 2011.