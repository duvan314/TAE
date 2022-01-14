rm(list = ls())
library(tidyverse)
library(lubridate)
library(kableExtra)
library(knitr)

# Carga de datos ----------------------------------------------------------

df <- readxl::read_excel("../Datos/registros_autos_entrenamiento.xlsx")
colnames(df) <- c("Date", "Units")

holiday <- readxl::read_excel("../Datos/FESTIVOS.xlsx")
holiday$Date <- ymd(holiday$Date)

results <- read.csv("../Datos/results.csv", encoding = "UTF-8")
results$date <- ymd(results$date)

results %>%  
  filter(year(date)>2011, year(date)<=2018) %>%
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
  
  numerador <- (Y-Y_hat)^2
  denominador <- (Y - mean(Y))^2
  PseudoR2_0 <-1 - (sum(numerador)/sum(denominador))
  
  return(PseudoR2_0)
}




Result <- function(fit, Y_train, pred, Y_test){
  Y_train <- train$Units
  Y_test  <- test$Units
  ECM_train <- ECM(fit, train$Units)
  ECM_test <- ECM(pred, test$Units)
  
  ECM_t <- c(ECM_train,ECM_test,ECM_test / ECM_train)
  
  R2_train <- R2(fit, train$Units)
  R2_test <- R2(pred, test$Units)
  R2_t <- c(R2_train, R2_test, R2_train/R2_test)
  
  d <- data.frame(ECM_t, R2_t)
  rownames(d) = c("Train", "Test", "Ratio")
  colnames(d) = c("ECM", "R2")
  return(t(d) %>% kbl(digits = 3) %>%  kable_styling(font_size = 12))
}


## Variable para agregar 

f <- function(x){
  if(x<=10){
    y <- -(0.5*x-5)^2
  } else if (x>20){
    y <- (0.5*x-10)^2
  }else{
    y <- 0
  }
  return(y)
}

plot(dv_dummy(1:30))


dv_dummy <- Vectorize(f)


# Guardar Objetos ---------------------------------------------------------

saveRDS(df, "../Datos/data.rds")         # datos de entreno
saveRDS(data_2018, "../Datos/data_2018.rds")   # datos de prueba


# Guardar funciones -------------------------------------------------------

saveRDS(ECM, "../Funciones/ECM.rds")
saveRDS(R2, "../Funciones/R2.rds")
saveRDS(Result, "../Funciones/Result.rds")
saveRDS(dv_dummy, "../Funciones/dv_dummy.rds")
saveRDS(dv_dummy_y, "../Funciones/dv_dummy_y.rds")


## Continua en Informe.Rmd
