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

# GRAVEDAD ------------------------------------------------------------------
datos %>% group_by(GRAVEDAD) %>% summarise(n = n())
datos <- datos %>% mutate(GRAVEDAD = ifelse(GRAVEDAD=="Solo da\\xF1os","Solo daños",GRAVEDAD))
datos %>% group_by(GRAVEDAD) %>% summarise(n = n())

# Crear LONGITUD y LATITUD ------------------------------------------------------

datos <- datos %>%  mutate(LONGITUD = format(as.numeric(str_extract(LOCATION, "-75[.][0-9]+")),digits=15),
                LATITUD = format(str_extract(LOCATION, "6[.][0-9]+"),digits=15))

# Seleccion de variables relevantes ---------------------------------------
datos <- datos %>% select(CLASE_ACCIDENTE, DIRECCION, DISEÑO, GRAVEDAD, 
                          NUMCOMUNA, COMUNA, BARRIO, LONGITUD,LATITUD, FECHA_ACCIDENTES)

names(datos)       # que variables tenemos?
apply(is.na(datos), 2, sum)[apply(is.na(datos), 2, sum)>0]    # que varaibles contienen NA?


# comunas -----------------------------------------------------------------
datos %>% group_by(COMUNA) %>% summarise(n = n())
datos <- datos %>% mutate(COMUNA = ifelse(COMUNA=="No Georef",NA,COMUNA),
                          COMUNA = ifelse(COMUNA=="Sin Inf",NA,COMUNA))
datos %>% group_by(COMUNA) %>% summarise(n = n())

# ## descartaremos los corregimientos -------------------------------------
nrow(datos)
datos <- datos %>% filter(!str_detect(COMUNA, "^Correg"))  # elimina 270765-244068=26697
datos%>% group_by(COMUNA) %>% summarise(n = n())
nrow(datos)

# ## Reordenando COMUNA -------------------------------------
datos %>% mutate(COMUNA = ifelse(str_detect(datos$COMUNA, "^Bel"), "Belén", COMUNA),
                      COMUNA = ifelse(str_detect(datos$COMUNA, "La Am"), "La América", COMUNA),
                      COMUNA = ifelse(COMUNA%in%c("0", "AU", "In", "SN"), NA, COMUNA)) -> datos
datos%>% group_by(COMUNA) %>% summarise(n = n())

datos %>% filter(is.na(COMUNA)) %>% group_by(NUMCOMUNA) %>% summarise(n = n())

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
       # ELIMINAN: 244068-242871=1197

datos %>% filter(is.na(BARRIO)) %>% group_by(LATITUD, LONGITUD) %>% 
  summarise(n=n()) %>% arrange(-n) 


rm(barrio, datos_barrios, datos2, DF, barrios, BARRIO)





# Falta reorganizar mejor los barrios  ------------------------------------

datos <- datos %>% select(-barrio)


# VARIABLES  --------------------------------------------------------------

names(datos)

# A trabajar con los barrios  ---------------------------------------------

data.frame(table(datos$BARRIO))->barrios


# ordenar barrios ---------------------------------------------------------
# AUC2, AUC1

datos %>% filter(!(BARRIO %in% c("AUC1","AUC2"))) -> datos  # se eliminan 4 registros 


# ## Eliminar AUC 1 y 2 ---------------------------------------------------
data.frame(table(datos$BARRIO))->barrios


# arraglar barrios --------------------------------------------------------

datos %>% 
  mutate(BARRIO = ifelse(BARRIO=="Aldea Pablo V","Aldea Pablo VI",BARRIO),
         BARRIO = ifelse(BARRIO=="Alejandr\\xEDa","Alejandría",BARRIO),
         BARRIO = ifelse(BARRIO=="Alejandro Echavarr\\xEDa","Alejandro Echavarría",BARRIO),
         BARRIO = ifelse(BARRIO=="Alfonso L\\xF3pez","Alfonso López",BARRIO),
         BARRIO = ifelse(BARRIO=="Altos de El Poblado","Altos del Poblado",BARRIO),
         BARRIO = ifelse(BARRIO=="Andaluc\\xEDa","Andalucía",BARRIO),
         BARRIO = ifelse(BARRIO=="Antonio Nari\\xF1o","Antonio Nariño",BARRIO),
         BARRIO = ifelse(BARRIO=="Asomadera N","Asomadera No. 1",BARRIO),
         BARRIO = ifelse(BARRIO=="Aures N","Aures No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Aures No. 2","Aures No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="B. Cerro  El Volador","B. Cerro El Volador",BARRIO),
         BARRIO = ifelse(BARRIO=="Barrio Caycedo","Barrio Caicedo",BARRIO),
         BARRIO = ifelse(BARRIO=="Barrio Col\\xF3n","Barrio Colombia",BARRIO),
         BARRIO = ifelse(BARRIO=="Barrio Crist\\xF3bal","Barrio Cristóbal",BARRIO),
         BARRIO = ifelse(BARRIO=="Barrio de Jes\\xFAs","Barrio de Jesús",BARRIO),
         BARRIO = ifelse(BARRIO=="Barrios de Jesús","Barrio de Jesús",BARRIO),
         BARRIO = ifelse(BARRIO=="Batall\\xF3n Girardot","Batallón Girardot",BARRIO),
         BARRIO = ifelse(BARRIO=="Bel\\xE9n","Belén",BARRIO),
         BARRIO = ifelse(BARRIO=="Belalc\\xE1zar","Belalcázar",BARRIO),
         BARRIO = ifelse(BARRIO=="Berl\\xEDn","Berlin",BARRIO),
         BARRIO = ifelse(BARRIO=="Berlín","Berlin",BARRIO),
         BARRIO = ifelse(BARRIO=="Bombon\\xE1 No. 1","Bomboná No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Bomboná No. 1","Bomboná No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Bombon\\xE1 No.1","Bomboná No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Bombon\\xE1 No. 2","Bomboná No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Bomboná No. 2","Bomboná No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Boyac\\xE1","Boyacá",BARRIO),
         BARRIO = ifelse(BARRIO=="C\\xF3rdoba","Córdoba",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Vald\\xE9s No. 1","Campo Valdés No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Valdés No. 1","Campo Valdés No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Vald\\xE9s No. 2","Campo Valdés No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Vald\\xE9s No.2","Campo Valdés No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Valdés No. 2","Campo Valdés No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Campo Valdés N","Campo Valdés No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Catalu\\xF1a","Cataluña",BARRIO),
         BARRIO = ifelse(BARRIO=="Coraz\\xF3n de Jes\\xFAs","Corazón de Jesús",BARRIO),
         BARRIO = ifelse(BARRIO=="Diego Echavarr\\xEDa","Diego Echavarría",BARRIO),
         BARRIO = ifelse(BARRIO=="El Coraz\\xF3n","El Corazón",BARRIO),
         BARRIO = ifelse(BARRIO=="El Rinc\\xF3n","El Rincón",BARRIO),
         BARRIO = ifelse(BARRIO=="El Vel\\xF3dromo","El Velódromo",BARRIO),
         BARRIO = ifelse(BARRIO=="Estaci\\xF3n Villa","Estación Villa",BARRIO),
         BARRIO = ifelse(BARRIO=="F\\xE1tima","Fátima",BARRIO),
         BARRIO = ifelse(BARRIO=="H\\xE9ctor Abad G\\xF3mez","Héctor Abad Gómez",BARRIO),
         BARRIO = ifelse(BARRIO=="Hospital San Vicente de Pa\\xFAl","Hospital San Vicente de Paúl",BARRIO),
         BARRIO = ifelse(BARRIO=="Jard\\xEDn Bot\\xE1nico","Jardín Botánico",BARRIO),
         BARRIO = ifelse(BARRIO=="Inst",NA,BARRIO),
         BARRIO = ifelse(BARRIO=="Jes\\xFAs Nazareno","Jesús Nazareno",BARRIO),
         BARRIO = ifelse(BARRIO=="La Am\\xE9rica","LA América",BARRIO),
         BARRIO = ifelse(BARRIO=="L\\xF3pez de Mesa","López de Mesa",BARRIO),
         BARRIO = ifelse(BARRIO=="La Loma de los Bernal","La Loma de Los Bernal",BARRIO),
         BARRIO = ifelse(BARRIO=="La Mansi\\xF3n","La Mansión",BARRIO),
         BARRIO = ifelse(BARRIO=="La mota","La Mota",BARRIO),
         BARRIO = ifelse(BARRIO=="La Pi\\xF1uela","La Pilarica",BARRIO),
         BARRIO = ifelse(BARRIO=="Los \\xC1ngeles","Los Alcázares",BARRIO),
         BARRIO = ifelse(BARRIO=="Los Alc\\xE1zares","Los Alcázares",BARRIO),
         BARRIO = ifelse(BARRIO=="Manrique Central No. 1","Manrique Central No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Manrique Central N","Manrique Central No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Manrique Central No. 2","Manrique Central No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Mar\\xEDa Cano Carambolas","María Cano Carambolas",BARRIO),
         BARRIO = ifelse(BARRIO=="Mosc\\xFA No. 1","Moscú No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Moscú No. 1","Moscú No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Mosc\\xFA No. 2","Moscú No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Mosc\\xFA No.2","Moscú No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Moscú No. 2","Moscú No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Moscú","Moscú No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Nueva Villa de Aburr\\xE1","Nueva Villa de Aburrá",BARRIO),
         BARRIO = ifelse(BARRIO=="Nueva Villa de la Iguan\\xE1","Nueva Villa de la Iguaná",BARRIO),
         BARRIO = ifelse(BARRIO=="Nueva Villa de La Iguaná","Nueva Villa de la Iguaná",BARRIO),
         BARRIO = ifelse(BARRIO=="Play\\xF3n de Los Comuneros","Playón de los Comuneros",BARRIO),
         BARRIO = ifelse(BARRIO=="Playón de Los Comuneros","Playón de los Comuneros",BARRIO),
         BARRIO = ifelse(BARRIO=="San Germ\\xE1n","San Germán",BARRIO),
         BARRIO = ifelse(BARRIO=="San Joaqu\\xEDn","San Joaquín",BARRIO),
         BARRIO = ifelse(BARRIO=="San Jos\\xE9 la Cima No. 1","San José la Cima No. 1",BARRIO),
         BARRIO = ifelse(BARRIO=="San Jos\\xE9 la Cima No.2","San José la Cima No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="San Mart\\xEDn de Porres","San Martín de Porres",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa F","Santa Fé",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa F\\xE9","Santa Fé",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa In\\xE9s","Santa Inés",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa Luc\\xEDa","Santa Lucía",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa M\\xF3nica","Santa Margarita",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa Mar\\xEDa de los \\xC1ngeles","Santa María de los Ángeles",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa Mar\\xEDa de Los \\xC1ngeles","Santa María de los Ángeles",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa Maria de los Angeles","Santa María de los Ángeles",BARRIO),
         BARRIO = ifelse(BARRIO=="Santa María de Los Ángeles","Santa María de los Ángeles",BARRIO),
         BARRIO = ifelse(BARRIO=="Santo Domingo Savio No. 1","Santo Domingo Savio No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Sim\\xF3n Bol\\xEDvar","",BARRIO),
         BARRIO = ifelse(BARRIO=="Simon Bolivar","Simón Bolívar",BARRIO),
         BARRIO = ifelse(BARRIO=="Sin Nombre","El Pesebre",BARRIO),
         BARRIO = ifelse(BARRIO=="Versalles No. 1","Versalles No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Versalles No. 2","Versalles No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Villa Lilliam","Villa Lilian",BARRIO),
         BARRIO = ifelse(BARRIO=="Villa Liliam","Villa Lilian",BARRIO),
         BARRIO = ifelse(BARRIO=="Bomboná" & LATITUD=="-75.5610220362","Bomboná No.1",BARRIO),
         BARRIO = ifelse(BARRIO=="Bomboná" & LATITUD=="-75.5415861489","Bomboná No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Versalles N" & LATITUD=="6.26646775929","Versalles No.2",BARRIO),
         BARRIO = ifelse(BARRIO=="Versalles N" & LATITUD=="6.26332153172 ","Versalles No.1",BARRIO)
         )-> datos


data.frame(table(datos$BARRIO)) %>% nrow()   # 271 barrios



# Filtrar barrios con BA --------------------------------------------------

datos %>% filter(!is.na(datos$BARRIO))-> datos

# escribir datos ----------------------------------------------------------

write.csv(datos, file = "../datos/datos_listos.csv")



# mas sobre los barrios: NA -----------------------------------------------

data.frame(table(datos$BARRIO)) -> barrios
datos %>% 
  mutate(BARRIO = ifelse(COMUNA=="El Poblado"& is.na(BARRIO),"Santa María de los Ángeles",BARRIO),
         COMUNA = ifelse(COMUNA=="El Poblado"& BARRIO == "sANTA fé","Guayabal",COMUNA))->datos 


datos %>% 
  filter(is.na(BARRIO)) %>% group_by(COMUNA) %>% 
  summarise(n=n())


datos %>% 
  filter(COMUNA=="Guayabal") %>% filter(is.na(BARRIO)) %>% group_by(LATITUD, LONGITUD) %>% 
  summarise(n=n())




datos %>% 
  filter(COMUNA=="Guayabal") %>% 
  mutate(i = ifelse(is.na(BARRIO), 2, 0.5)) %>% 
  ggplot()+
  geom_jitter(aes(LATITUD, LONGITUD, color = BARRIO, size = i))



