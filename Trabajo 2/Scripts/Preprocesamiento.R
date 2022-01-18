rm(list = ls())
library(tidyverse)
library(lubridate)
library(kableExtra)
library(knitr)
library(openxlsx)

# Carga de datos ----------------------------------------------------------

df <- readxl::read_excel("../Datos/registros_autos_entrenamiento.xlsx")
colnames(df) <- c("Date", "Units")

holiday <- readxl::read_excel("../Datos/FESTIVOS.xlsx")
holiday$Date <- ymd(holiday$Date)

results <- read.csv("../Datos/results.csv", encoding = "UTF-8")
results$date <- ymd(results$date)

results %>%  
  filter(lubridate::year(date)>2011, lubridate::year(date)<=2018) %>%
  filter(home_team == "Colombia" | away_team == "Colombia") %>% 
  filter(home_team == "Colombia" | tournament != "Friendly") %>% 
  dplyr::select(date) %>% mutate(Colombia = 1) %>% 
  rename(Date = date) -> results

left_join(df, holiday,by = "Date") %>% 
  mutate(Holiday = as.factor(ifelse(is.na(Holiday),0,1))) %>% 
  left_join(results, by = "Date") %>% 
  mutate(Colombia = as.factor(ifelse(is.na(Colombia),0,1))) -> df

apply(is.na(df), 2,sum)

# Tratamiento Fechas ------------------------------------------------------


df$Date <- ymd(df$Date)

df %>% mutate(Year = year(Date),
              Month = month(Date,label = TRUE, abbr = FALSE),
              Wday = weekdays(Date),
              Mday = day(Date),
              Yday = yday(Date)) -> df

df$Wday <- factor(df$Wday, levels  = c("lunes","martes","miércoles","jueves","viernes","sábado","domingo"))



str(df)


# datos validación 2018 ---------------------------------------------------

Date <- seq(as.Date("2018/1/1"), by = "day", length.out = 365)
data_2018 <- data.frame(Date)
data_2018 %>% left_join(holiday) %>% left_join(results) %>% 
  mutate(Colombia = as.factor(ifelse(is.na(Colombia),0,1)),
         Holiday = as.factor(ifelse(is.na(Holiday),0,1)),
         Year = year(Date),
         Month = month(Date,label = TRUE, abbr = FALSE),
         Wday = weekdays(Date),
         Mday = day(Date),
         Yday = yday(Date)) -> data_2018
  
names(data_2018)
names(df)




# Funciones ---------------------------------------------------------------

ECM <- function(Y_hat, Y){
  return(mean((Y-Y_hat)^2))
}

R2 <- function(Y_hat, Y){
  
  r2 <- 1-(sum((Y-Y_hat)^2)/
             sum((Y-mean(Y))^2))
  
  
  return(r2)
}




Result <- function(fit, Y_train, pred, Y_test){

  ECM_train <- ECM(fit, Y_train)
  ECM_test <- ECM(pred, Y_test)
  
  ECM_t <- c(ECM_train,ECM_test,ECM_test / ECM_train)
  
  R2_train <- R2(fit, Y_train)
  R2_test <- R2(pred, Y_test)
  R2_t <- c(R2_train, R2_test, R2_train/R2_test)
  
  d <- data.frame(ECM_t, R2_t)
  rownames(d) = c("Train", "Test", "Ratio")
  colnames(d) = c("ECM", "R2")
  return(t(d) %>% kbl(digits = 3) %>%  kable_styling(font_size = 12))
}


## Variable para agregar 

f <- function(x){
  if(x<=10){
    y <- -(0.5*x-5)^4
  } else if (x>20){
    y <- (0.5*x-10)^4
  }else{
    y <- 0
  }
  return(y)
}



dv_dummy <- Vectorize(f)


# Guardar Objetos ---------------------------------------------------------

saveRDS(df, "../Datos/data.rds")         # datos de entreno
saveRDS(data_2018, "../Datos/data_2018.rds")   # datos de prueba
write.xlsx(data_2018, file = "../Datos/data_2018.xlsx")


# Guardar funciones -------------------------------------------------------

saveRDS(ECM, "../Funciones/ECM.rds")
saveRDS(R2, "../Funciones/R2.rds")
saveRDS(Result, "../Funciones/Result.rds")
saveRDS(dv_dummy, "../Funciones/dv_dummy.rds")


## Continua en Informe.Rmd
