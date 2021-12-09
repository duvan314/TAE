rm(list = ls())
source("Datos_Mod_diarios.R", encoding = "UTF-8")

# modelo diario -----------------------------------------------------------
table(datos$CLASE_ACCIDENTE)
diario_df <- datos_diarios(clase_accidente = "Caída Ocupante")


### modelo ridge ------------------------------------------------------------

x <- model.matrix(CASOS ~ ., diario_df)[, -1]   # matriz de modelos   
y <- diario_df$CASOS                                # variable dependiente 
train <- x[,"AÑO"] <2018
test <- x[,"AÑO"] >= 2018 &  x[,"AÑO"] < 2020
x_train <- x[train,]
x_test <- x[test,]
y_train <- y[train]
y_test <- y[test]

gridz <- 10^seq(-2, 5, length = 100)  ## grilla posibles valores de lambda

### entreno ridge 

mod_ridge_diarios <- glmnet(x_train, y_train, alpha = 0, lambda = gridz)

cv_ridge<-cv.glmnet(x_train, y_train,alpha=0)    # cross validaton para escoger el lambda 
best_lambda_ridge <- cv_ridge$lambda.min
out_ridge <- glmnet (x_train, y_train,alpha=0)
coef_ridge <- predict(out_ridge, type = "coefficients", s = best_lambda_ridge) 

ridge_pred_train <- predict(mod_ridge_diarios, s = best_lambda_ridge, newx = x_train)
ridge_pred_test <- predict(mod_ridge_diarios, s = best_lambda_ridge, newx = x_test)
ridge_pred_2020 <- predict(mod_ridge_diarios, s = best_lambda_ridge, newx = x[x[, "AÑO"]==2020,])
ECM_ridge_train <- mean((ridge_pred_train - y_train)^2)
ECM_ridge_test <- mean((ridge_pred_test - y_test)^2)
ECM_ridge_2020 <- mean((ridge_pred_2020 - y[x[, "AÑO"]==2020])^2)
ridge <- c(ECM_ridge_train, ECM_ridge_test, ECM_ridge_2020)
### entreno lasso

mod_lasso_diarios <- glmnet(x_train, y_train, alpha = 1, lambda = gridz)

cv_lasso<-cv.glmnet(x_train, y_train,alpha=1)    # cross validaton para escoger el lambda 
best_lambda_lasso <- cv_ridge$lambda.min
out_lasso <- glmnet (x_train, y_train,alpha=1)
coef_lasso <- predict(out_ridge, type = "coefficients", s = best_lambda_lasso) 

lasso_pred_train <- predict(mod_lasso_diarios, s = best_lambda_lasso, newx = x_train)
lasso_pred_test <- predict(mod_lasso_diarios, s = best_lambda_lasso, newx = x_test)
lasso_pred_2020 <- predict(mod_lasso_diarios, s = best_lambda_lasso, newx = x[x[, "AÑO"]==2020,])
ECM_lasso_train <- mean((lasso_pred_train - y_train)^2)
ECM_lasso_test <- mean((lasso_pred_test - y_test)^2)
ECM_lasso_2020 <- mean((lasso_pred_2020 - y[x[, "AÑO"]==2020])^2)
lasso <- c(ECM_lasso_train, ECM_lasso_test, ECM_lasso_2020)

data.frame(ridge, lasso)



# Poisson model -----------------------------------------------------------

datos_train <- diario_df %>% filter(AÑO %in% c(2014,2015,2016,2017))
datos_test <- diario_df %>% filter(AÑO %in% c(2018,2019))
datos_2020 <- diario_df %>% filter(AÑO %in% c(2020))


mod_poisson_diarios  <- glm(CASOS ~ ., data = datos_train, family = poisson(link = "log"))

poisson_predit_train <- predict(mod_poisson_diarios, newdata = datos_train,type = "response")
poisson_predit_test <- predict(mod_poisson_diarios, newdata = datos_test,type = "response")
poisson_predit_2020 <- predict(mod_poisson_diarios, newdata = datos_2020,type = "response")

ECM_poisson_train <- mean((poisson_predit_train - datos_train$CASOS)^2)
ECM_poisson_test <- mean((poisson_predit_test - datos_test$CASOS)^2)
ECM_poisson_2020 <- mean((poisson_predit_2020 - datos_2020$CASOS)^2)
poisson <- c(ECM_poisson_train, ECM_poisson_test, ECM_poisson_2020)

# randonforest 



datos_train <- diario_df %>% filter(AÑO %in% c(2014,2015,2016,2017))
datos_test <- diario_df %>% filter(AÑO %in% c(2018,2019))
datos_2020 <- diario_df %>% filter(AÑO %in% c(2020))


set.seed(111)
diarios_rp <- rpart(CASOS ~ .,data = datos_train)


diarios_rp$cptable

ECM_rf_train <- mean((predict(diarios_rp, newdata = datos_train)-datos_train$CASOS)^2)
ECM_rf_test <-mean((predict(diarios_rp, newdata = datos_test)-datos_test$CASOS)^2)
ECM_rf_2020 <-mean((predict(diarios_rp, newdata = datos_2020)-datos_2020$CASOS)^2)
rp <- c(ECM_rf_train, ECM_rf_test, ECM_rf_2020)

ECM_Atropello_diarios <- data.frame(ridge, lasso, poisson, rp)# modelo ganador, poisson. 


ECM_Atropello_diarios   # modelo ganardor, rp




poisson_mod <- function(train){
  poisson <- train(CASOS ~., data = train, method = "glm", family = "poisson")
  
  return(poisson)
}



poisson_model_diarios <-poisson_mod(datos_train)


saveRDS(poisson_model_diarios,"../Modelos diarios/posson_caidaOcupante.rds")





