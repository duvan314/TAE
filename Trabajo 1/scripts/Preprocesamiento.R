# bORRAR mEMORIA ----------------------------------------------------------
rm(list = ls())
# librerias ---------------------------------------------------------------
setwd("C:/Users/DUVAN/OneDrive/1 U/2021-2/TAE/Trabajo 1/scripts")
library(tidyverse); library(lubridate); library(readxl)

# datos -------------------------------------------------------------------
datos <- read_excel("../datos/incidentes_viales.xlsx")

# CLASE ACCIDENTE ---------------------------------------------------------
datos %>% group_by(CLASE_ACCIDENTE) %>% summarise(n = n())

# reagrupar 
datos <- datos %>% mutate(CLASE_ACCIDENTE = ifelse(str_detect(datos$CLASE_ACCIDENTE, "^Ca(i|í)"),
                                                   "Caída Ocupante",CLASE_ACCIDENTE),
                          CLASE_ACCIDENTE = ifelse(is.na(datos$CLASE_ACCIDENTE), "Otro", CLASE_ACCIDENTE))


datos %>% group_by(CLASE_ACCIDENTE) %>% summarise(n = n()) %>% arrange(-n)

# DISEÑO ------------------------------------------------------------------
datos %>% group_by(DISEÑO) %>% summarise(n = n())
datos <- datos %>% mutate(DISEÑO = ifelse(is.na(DISEÑO),"Otro",DISEÑO),
                          DISEÑO = ifelse(DISEÑO=="Pont\\xF3n" , "Pontón", DISEÑO))
datos %>% group_by(DISEÑO) %>% summarise(n = n())

# Crear LONGITUD y LATITUD ------------------------------------------------------

datos <- datos %>%  mutate(LONGITUD = format(as.numeric(str_extract(LOCATION, "-75[.][0-9]+")),digits=15),
                LATITUD = format(str_extract(LOCATION, "6[.][0-9]+"),digits=15))

# Seleccion de variables relevantes ---------------------------------------

datos <- datos %>% select(CLASE_ACCIDENTE, DIRECCION, DISEÑO, FATALIDAD, 
                          NUMCOMUNA, COMUNA, BARRIO, LONGITUD,LATITUD, FECHA_ACCIDENTES)

names(datos)       # que variables tenemos?
apply(is.na(datos), 2, sum)[apply(is.na(datos), 2, sum)>0]    # que varaibles contienen NA?


# comunas -----------------------------------------------------------------
datos %>% group_by(COMUNA) %>% summarise(n = n())
datos <- datos %>% mutate(COMUNA = ifelse(COMUNA=="No Georef",NA,COMUNA),
                          COMUNA = ifelse(COMUNA=="Sin Inf",NA,COMUNA))
datos %>% group_by(COMUNA) %>% summarise(n = n())

# ## descartaremos los corregimientos -------------------------------------
datos <- datos %>% filter(!str_detect(COMUNA, "^Correg"))
datos%>% group_by(COMUNA) %>% summarise(n = n())

# ## Reordenando COMUNA -------------------------------------
datos %>% mutate(COMUNA = ifelse(str_detect(datos$COMUNA, "^Bel"), "Belén", COMUNA),
                      COMUNA = ifelse(str_detect(datos$COMUNA, "La Am"), "La América", COMUNA),
                      COMUNA = ifelse(COMUNA%in%c("0", "AU", "In", "SN"), NA, COMUNA)) -> datos
datos%>% group_by(COMUNA) %>% summarise(n = n())

# Cuantos BARRIOS con NA? -------------------------------------------------

sum(is.na(datos$BARRIO))      #    172

# Aunque relativamente son pocos los barrios con NA, si hay muchos que no tienen un nombre correcto


# Barrios con codigos -----------------------------------------------------

datos_barrios <- read_csv("../datos/comunas.txt",col_names = FALSE)
barrios<-str_extract(datos_barrios$X1,"[0-9]+\\s\\W\\s[a-zA-Z].+[A-Za-z].?|\\s\\d?")
BARRIO <- str_extract(barrios,"[0-9]+")

barrio <- str_extract(barrios,"([a-zA-Z].+\\s*){1,}")
barrio <- str_extract(barrios,"([a-zA-Z].+\\s*){1,}[a-zA-Z].\\S")
barrios <- data.frame(BARRIO, barrio)

datos %>% filter(!is.na(BARRIO)) %>% filter(BARRIO!="0") %>% 
  filter(str_detect(BARRIO, "^[0-9]+"))->DF


datos2 <- left_join(DF, barrios)
datos <- left_join(datos, datos2)

datos %>% nrow()
datos %>% mutate(BARRIO = ifelse(str_detect(BARRIO, "^[0-9]+"), barrio, BARRIO))->datos

datos %>% filter(is.na(BARRIO)) %>% group_by(LATITUD, LONGITUD) %>% 
  summarise(n=n()) %>% arrange(-n) 

datos <- datos %>% filter(!(datos$LATITUD=="6.22141524356" & datos$LONGITUD== "-75.7037762763"))

datos %>% filter(is.na(BARRIO)) %>% group_by(LATITUD, LONGITUD) %>% 
  summarise(n=n()) %>% arrange(-n) 


rm(barrio, datos_barrios, datos2, DF, barrios, BARRIO)





# Falta reorganizar mejor los barrios  ------------------------------------

datos <- datos %>% select(-barrio)


# A trabajar con los barrios  ---------------------------------------------

data.frame(table(datos$BARRIO))

# escribir datos ----------------------------------------------------------

#write.csv(datos, file = "../datos/datos_listos.csv")
