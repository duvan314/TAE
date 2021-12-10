rm(list = ls())
library(tidyverse)
source("../scripts/Datos_Mod_diarios.R", encoding = "UTF-8")
# predicciones ------------------------------------------------------------


predicciones <- function(clase_acc, temporalid){
  
  if (clase_acc == "Atropello") {
    modelo <- readRDS("../Modelos diarios/posson_model_atropello.rds")
    
  }else if (clase_acc == "Caída Ocupante") {
    modelo <- readRDS("../Modelos diarios/posson_caidaOcupante.rds")
    
  }else if (clase_acc == "Choque") {
    modelo <- readRDS("../Modelos diarios/ridge_model_choque.rds")
  } else{
    modelo <-  readRDS("../Modelos diarios/ridge_model_Otros.rds")    # solo se deben permitir esas 4 clases 
    
  }
  
  

datos_test <- datos_diarios(clase_acc) %>% filter(AÑO %in% c(2018,2019))


if (temporalid == "diaria") {
  pre_diarios <- datos_test %>%
    mutate(predit=predict(modelo, datos_test)) %>% 
    group_by(AÑO, MES, DIA_MES) %>% 
    summarise(casos=sum(predit)) %>% 
    mutate(casos = ifelse(casos<0, 0, casos)) %>% 
    mutate(fecha = ymd(paste(AÑO,"-",MES,"-",DIA_MES))) %>%
    ungroup() %>% 
    select(fecha, casos) 
  
  
  return(pre_diarios)
  
} else if (temporalid == "semanal") {
  
  pre_semanal <- datos_test %>%
    mutate(predit=predict(modelo, datos_test)) %>% 
    group_by(AÑO, MES, DIA_MES, SEMANA) %>% 
    summarise(casos=sum(predit)) %>% ungroup() %>% 
    mutate(fecha = ymd(paste(AÑO,"-",MES,"-",DIA_MES))) %>%
    ungroup() %>% 
    select(fecha, SEMANA, casos) %>%
    group_by(year(fecha), SEMANA) %>% 
    summarise(fecha = min(fecha), casos = sum(casos)) 
  
  return(pre_semanal)
  
} else if  (temporalid == "mensual") {
  
  
  pre_mensual <- datos_test %>%
    mutate(predit=predict(modelo, datos_test)) %>% 
    group_by(AÑO, MES) %>% 
    summarise(casos=sum(predit)) %>% ungroup() %>% 
    mutate(fecha = ym(paste(AÑO,"-",MES))) %>%
    ungroup() %>% 
    select(fecha, casos)
  
  return(pre_mensual)
  
  
} else{
  
  pre_anual <- datos_test %>%
    mutate(predit=predict(modelo, datos_test)) %>% 
    group_by(AÑO) %>% rename(fecha=AÑO) %>% 
    summarise(casos=sum(predit)) %>% ungroup()
  return(pre_anual)
}




}



# plot --------------------------------------------------------------------

plot_prediccion <- function(clase_acc, temporalid, t_min, t_max){
  
  prediccion <- predicciones(clase_acc, temporalid)
  
  
  t_min <- ymd(t_min)
  t_max <- ymd(t_max)
  
  
  if(t_min > t_max){
    prediccion %>% 
      ggplot() + geom_bar(aes(fecha), fill = "white")+
      ggtitle("NA: Fechas incorrectas") + theme_classic()
  } else if (temporalid == "anual") {
    
    ## anual 
    prediccion %>% filter(fecha>=year(t_min) & fecha<=year(t_max)) %>% 
      mutate(fecha = as.factor(fecha)) %>% 
      ggplot(aes(fecha,casos,fill=fecha))+
      ggtitle(paste("Prediccion", temporalid, "de accidentes por", clase_acc, "(",
                    t_min,"a", t_max,")"))+
      geom_bar(stat="identity",position="dodge")
    
  }else{
    ## diario, semanal y mensual 
    if (nrow(prediccion %>% filter(fecha>=t_min & fecha<=t_max))<=1 ){
      
      prediccion %>% filter(fecha>=t_min & fecha<=t_max)%>%  
      ggplot(aes(fecha, casos))+geom_point(color = "blue", size = 3)+ theme_classic()+
        ggtitle(paste("Prediccion", temporalid, "de accidentes por", clase_acc, "(",
                      t_min,"a", t_max,")"))+
      theme(panel.background = element_rect(fill = "lightblue"))
    
  } else{
    prediccion %>% filter(fecha>=t_min & fecha<=t_max)%>%  
      ggplot(aes(fecha, casos))+geom_point(color = "blue", size = 3)+
      geom_line(color = "blue3")+ theme_classic()+
      ggtitle(paste("Prediccion", temporalid, "de accidentes por", clase_acc, "(",
                    t_min,"a", t_max,")"))+
      theme(panel.background = element_rect(fill = "lightblue"))
  }
  
  
  }
  
}



# plot ejecutar -----------------------------------------------------------



#clase_acc <-  "Atropello"   # c( "Atropello", "Caída Ocupante", "Choque", "Otro" )
#temporalid <- "semanal"  # c("diaria", "semanal", "mensual", "anual")
#t_min <- "2018-02-01"    # año-mes-dia
#t_max <- "2019-12-01"    # año-mes-dia

#plot <- plot_prediccion(clase_acc,temporalid,t_min,t_max)
#plot



