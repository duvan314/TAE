rm(list = ls())
library(readxl)
library(tidyverse)
library(lubridate)
library(caret)
library(fastDummies)
require(randomForest)
library(ridge)
library(ranger)
library(psych)
library(glmnet)
library(rpart)
require(randomForest)

# -------------------------------------------------------------------------

datos <- read.csv("../datos/datos_listos.csv")
str(datos)
# datos diarios -----------------------------------------------------------

# datos especiales --------------------------------------------------------

FESTIVOS <- read.csv("../datos/FESTIVOS.csv")
FESTIVOS <-  FESTIVOS%>% mutate(FESTIVO = 1)
FESTIVOS$FECHA <- dmy(FESTIVOS$FECHA)

FERIAS  <- readxl::read_excel("../datos/Ferias.xlsx")
FERIAS<-  FERIAS%>% mutate(FERIA = 1)

FERIAS$FECHA <- ymd(FERIAS$FECHA)
str(FERIAS)


# -------------------------------------------------------------------------


clases <- c( "Atropello", "Caída Ocupante", "Choque", "Otro" )

datos_diarios <- function(clase_accidente){

  datos$FECHA_ACCIDENTE <- as.Date(datos$FECHA_ACCIDENTE)
  
  
datos %>%
  select(FECHA_ACCIDENTE,CLASE_ACCIDENTE, GRAVEDAD, DISEÑO,  AÑO, MES,DIA, SEMANA, DIA_MES, HORA) %>% 
  group_by(FECHA_ACCIDENTE, CLASE_ACCIDENTE,GRAVEDAD, DISEÑO,AÑO,MES,DIA, SEMANA, DIA_MES) %>%
  summarise( CASOS = n()) %>%
  ungroup()  %>% 
  full_join(FERIAS,by = c("FECHA_ACCIDENTE" = "FECHA")) %>% 
  full_join(FESTIVOS,by = c("FECHA_ACCIDENTE" = "FECHA")) %>% 
  mutate(FERIA = ifelse(is.na(FERIA), 0, 1)) %>% 
  mutate(FESTIVO = ifelse(is.na(FESTIVO), 0, 1)) %>% 
  select(-FECHA_ACCIDENTE) %>%
  filter(CLASE_ACCIDENTE == clase_accidente) %>%
  select(-CLASE_ACCIDENTE) -> datos_modelo

return(datos_modelo)

}






# -------------------------------------------------------------------------
