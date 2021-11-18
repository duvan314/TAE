rm(list = ls())  # borrar memoria

# librerias ---------------------------------------------------------------
library(tidyverse); library(lubridate); library(readxl)

# datos -------------------------------------------------------------------
datos <- read_excel("../datos/incidentes_viales.xlsx")

# CLASE ACCIDENTE ---------------------------------------------------------
datos %>% group_by(CLASE_ACCIDENTE) %>% summarise(n = n())
datos <- datos %>% mutate(CLASE_ACCIDENTE = ifelse(str_detect(datos$CLASE_ACCIDENTE, "^Ca(i|í)"),
                                                   "Caída Ocupante",CLASE_ACCIDENTE),
                          CLASE_ACCIDENTE = ifelse(CLASE_ACCIDENTE=="", "Otro", CLASE_ACCIDENTE),
                          CLASE_ACCIDENTE = ifelse(is.na(datos$CLASE_ACCIDENTE), "Otro", CLASE_ACCIDENTE))


datos %>% group_by(CLASE_ACCIDENTE) %>% summarise(n = n()) %>% arrange(-n)

datos$LOCATION

# diseño ------------------------------------------------------------------

datos %>% group_by(DISEÑO) %>% summarise(n = n())
datos <- datos %>% mutate(DISEÑO = ifelse(DISEÑO=="","Otro",DISEÑO),
                          DISEÑO = ifelse(DISEÑO=="Pont\\xF3n" , "Pontón", DISEÑO))

datos <- datos %>%  mutate(LONGITUD = format(as.numeric(str_extract(LOCATION, "-75[.][0-9]+")),digits=15),
                LATITUD = format(str_extract(LOCATION, "6[.][0-9]+"),digits=15))
names(datos)

datos <- datos %>% select(CLASE_ACCIDENTE, DIRECCION, DISEÑO, FATALIDAD, 
                          NUMCOMUNA, COMUNA, BARRIO, LONGITUD,LATITUD, FECHA_ACCIDENTE)

require(openxlsx)
write.xlsx(datos, file = "../datos/datos_listos.xlsx")
