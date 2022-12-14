#-------PROBLEM SET 2 

#-------Lectura bases 
train_hogares<-readRDS(here("../data/train_hogares.Rds"))
train_personas<-readRDS(here("../data/train_personas.Rds"))

#-------paquetes 
install.packages("glmnet")
require("here")
require("tidyverse")
require("glmnet")
install.packages("tidyverse")
install.packages("glmnet", repos = "https://cran.us.r-project.org")
install.packages("caret", dependencies = c("Depends", "Suggests"))
library(AppliedPredictiveModeling)
library(caret)
install.packages("ggplot2")
library (ggplot2)
library(glmnet)
library(pacman)
p_load(tidyverse, fastDummies, caret, glmnet, MLmetrics)
library(modeest)
library(dplyr)
library(tidyverse)
library(glmnet)
library(pacman)
p_load(AER, tidyverse, caret, MLmetrics, tidymodels, themis)


#----------------------------------- C o n s t r u c c i o n   d e   l a   b a s e ---------------------------------------
#Sexo jefe hogar
sex_jefe_hog<- as.data.frame(ifelse((train_personas$P6020==1 & train_personas$P6050==1),1,0))
train_personas<- cbind(train_personas, sex_jefe_hog)
names(train_personas)[names(train_personas)=='ifelse((train_personas$P6020 == 1 & train_personas$P6050 == 1), 1, 0)']<-'sexo_jefe_hogar'
sexo_jefe_hog<-train_personas %>% group_by(id) %>% summarize(sexo_jefe_hogar=sum(sexo_jefe_hogar,na.rm = TRUE)) 
summary(sexo_jefe_hog)
#Edad jefe hogar
edad_jefe_hogar<- (ifelse((train_personas$P6050==1),train_personas$P6040,0))
train_personas<- cbind(train_personas, edad_jefe_hogar)
edad_jefe_hog<-train_personas %>% group_by(id) %>% summarize(edad_jefe_hogar=sum(edad_jefe_hogar,na.rm = TRUE)) 
summary(edad_jefe_hog)

#Hacinamiento
hacinamiento=train_hogares$Nper/train_hogares$P5010
train_hogares<-cbind(train_hogares, hacinamiento)

#Régimen de salud jefe de hogar
rs_jefe_hogar<- (ifelse((train_personas$P6050==1),train_personas$P6100,0))
train_personas<- cbind(train_personas, rs_jefe_hogar)
rs_jefe_hog<-train_personas %>% group_by(id) %>% summarize(rs_jefe_hogar=sum(rs_jefe_hogar,na.rm = TRUE)) 
summary(rs_jefe_hog)
table(train_personas$P6050, train_personas$P6100)

#Nivel educativo jefe hogar
edu_jefe_hogar<- (ifelse((train_personas$P6050==1),train_personas$P6210,0))
train_personas<- cbind(train_personas, edu_jefe_hogar)
educ_jefe_hog<-train_personas %>% group_by(id) %>% summarize(edu_jefe_hogar=sum(edu_jefe_hogar,na.rm = TRUE)) 
summary(educ_jefe_hog)
table(train_personas$P6050, train_personas$P6210)

# Ocupación jefe hogar
ocupacion_jefe_hogar<- (ifelse((train_personas$P6050==1),train_personas$P6430,0))
train_personas<- cbind(train_personas, ocupacion_jefe_hogar)
ocupacion_jefe_hog<-train_personas %>% group_by(id) %>% summarize(ocupacion_jefe_hogar=sum(ocupacion_jefe_hogar,na.rm = TRUE)) 
summary(ocupacion_jefe_hog)
table(train_personas$P6050, train_personas$P6430)

#Subsidios
train_personas$P6585s1 = ifelse((train_personas$P6585s1==2),0,train_personas$P6585s1)
train_personas$P6585s2 = ifelse((train_personas$P6585s2==2),0,train_personas$P6585s2)
train_personas$P6585s3 = ifelse((train_personas$P6585s3==2),0,train_personas$P6585s3)
train_personas$P6585s4 = ifelse((train_personas$P6585s4==2),0,train_personas$P6585s4)
train_personas$P6585s1 = ifelse((train_personas$P6585s1==9),0,train_personas$P6585s1)
train_personas$P6585s2 = ifelse((train_personas$P6585s2==9),0,train_personas$P6585s2)
train_personas$P6585s3 = ifelse((train_personas$P6585s3==9),0,train_personas$P6585s3)
train_personas$P6585s4 = ifelse((train_personas$P6585s4==9),0,train_personas$P6585s4)
subsidio<-(train_personas$P6585s1+train_personas$P6585s2+train_personas$P6585s3+train_personas$P6585s4)
train_personas<-cbind(train_personas,subsidio)
sub_hog<-train_personas %>% group_by(id) %>% summarize(subsidio=sum(subsidio,na.rm = TRUE)) 

#Suma de horas trabajadas por hogar 
horas_tra_hogar<-train_personas %>% group_by(id) %>% summarize(P6800=mean(P6800,na.rm = TRUE)) 

#Suma de horas trabajadas segundo empleo por hogar 
horas_tra_hogar_2<-train_personas %>% group_by(id) %>% summarize(P7045=mean(P7045,na.rm = TRUE)) 


#-------Merge 
train_hogares<-left_join(train_hogares, sexo_jefe_hog)
train_hogares<-left_join(train_hogares, edad_jefe_hog)
train_hogares<-left_join(train_hogares, hacinamiento)
train_hogares<-left_join(train_hogares, rs_jefe_hog)
train_hogares<-left_join(train_hogares, educ_jefe_hog)
train_hogares<-left_join(train_hogares, ocupacion_jefe_hog)
train_hogares<-left_join(train_hogares, sub_hog)
train_hogares<-left_join(train_hogares, horas_tra_hogar)
train_hogares<-left_join(train_hogares, horas_tra_hogar_2)
colnames(train_hogares)


#-------Renonbrar variables y cambiar missings
names(train_hogares)[names(train_hogares)=='P5000']<-'Ncuartos'
names(train_hogares)[names(train_hogares)=='P5010']<-'Ncuartos_dormir'
names(train_hogares)[names(train_hogares)=='P5090']<-'Tipo_vivienda'
names(train_hogares)[names(train_hogares)=='P6800']<-'Horas_trabajo1'
names(train_hogares)[names(train_hogares)=='P7045']<-'Horas_trabajo2'
train_hogares$P5130 = ifelse(is.na(train_hogares$P5130)==T,0,train_hogares$P5130)
train_hogares$P5140 = ifelse(is.na(train_hogares$P5140)==T,0,train_hogares$P5140)
train_hogares$Horas_trabajo1 = ifelse(is.na(train_hogares$Horas_trabajo1)==T,0,train_hogares$Horas_trabajo1)
train_hogares$Horas_trabajo2 = ifelse(is.na(train_hogares$Horas_trabajo2)==T,0,train_hogares$Horas_trabajo2)

#-------Eliminar y crear variables 

train_hogares$subsidio<-(ifelse((train_hogares$subsidio>0),1,0))
arriendo_estimado<-train_hogares$P5130+train_hogares$P5140
train_hogares<-cbind(train_hogares,arriendo_estimado)

train_hogares<-train_hogares[,-2]
train_hogares<-train_hogares[,-6]
train_hogares<-train_hogares[,-6]
train_hogares<-train_hogares[,-6]
train_hogares<-train_hogares[,-8]
train_hogares<-train_hogares[,-10]
train_hogares<-train_hogares[,-12]
train_hogares<-train_hogares[,-13] 
train_hogares<-train_hogares[,-13]
train_hogares<-train_hogares[,-14]

#back up
train_hogares2<-train_hogares

#Logaritmo del ingreso percapita
ing_per = train_hogares2$Ingpcug
ing_per<-ifelse((ing_per)==0,1,ing_per)
log_ing_per<- log(ing_per)
train_hogares2<-cbind(train_hogares2,log_ing_per)
# 1.Variables como factor 
train_hogares2$Dominio<-as.factor(train_hogares2$Dominio)
train_hogares2$Tipo_vivienda<-as.factor(train_hogares2$Tipo_vivienda)
train_hogares2$rs_jefe_hogar<-as.factor(train_hogares2$rs_jefe_hogar)
train_hogares2$edu_jefe_hogar<-as.factor(train_hogares2$edu_jefe_hogar)
train_hogares2$ocupacion_jefe_hogar<-as.factor(train_hogares2$ocupacion_jefe_hogar)

test$Dominio<-as.factor(test$Dominio)
test$Tipo_vivienda<-as.factor(test$Tipo_vivienda)
train_hogares2$rs_jefe_hogar<-as.factor(train_hogares2$rs_jefe_hogar)
train_hogares2$edu_jefe_hogar<-as.factor(train_hogares2$edu_jefe_hogar)
train_hogares2$ocupacion_jefe_hogar<-as.factor(train_hogares2$ocupacion_jefe_hogar)
#----------------------------------- E s t a d í s t i c a s   D e s c r i p t i v a s ---------------------------------

table (train_hogares$Pobre) #131936  33024

#----------------------------------- D i v i s i ó n  d e  M u e s t r a ---------------------------------
set.seed(12345) 
train_hogares2 <- train_hogares2 %>%
  mutate(holdout= as.logical(1:nrow(train_hogares2) %in%
                               sample(nrow(train_hogares2), nrow(train_hogares2)*.2)))
test<-train_hogares2[train_hogares2$holdout==T,] #32.992
train<-train_hogares2[train_hogares2$holdout==F,] #131.968

#----------------------------------- M o d e l o s   d e  e s t i m a c i ó n ---------------------------------
#------------OLS
set.seed(12345)
ols <- train(log_ing_per~ Tipo_vivienda+ rs_jefe_hogar+edu_jefe_hogar+ Ncuartos + Ncuartos_dormir+ 
               Nper+Npersug+ hacinamiento+edad_jefe_hogar+Horas_trabajo1+Horas_trabajo2+
               arriendo_estimado+ocupacion_jefe_hogar+Dominio+subsidio+sexo_jefe_hogar, # model to fit
             data = train,
             trControl = trainControl(method = "cv", number = 10),
             method = "lm")

ols$results
#intercept      RMSE  Rsquared      MAE    RMSESD  RsquaredSD       MAESD
#1      TRUE 1.036285 0.3469095 0.538377 0.0333237 0.008536496 0.007743297

#----------Bases para predicciòn 
#train
x_continuas=(train[,c(3,4,6,7,14,18,22,24,26)])
x_categoricas=(train[,c(2,5,17,19,20,21,25)])
# Ahora procedemos a dummyficar la base
x_categoricas<- model.matrix(~ ., x_categoricas) %>%
  as.data.frame #%>%
predicciones_general<- cbind(x_categoricas,x_continuas)
predicciones_general<-predicciones_general[,-1]
predicciones_general<-as.matrix(predicciones_general)

#test
x_continuas_t=(test[,c(3,4,6,7,14,18,22,24,26)])
x_categoricas_t=(test[,c(2,5,17,19,20,21,25)])
# Ahora procedemos a dummyficar la base
x_categoricas_t<- model.matrix(~ ., x_categoricas_t) %>%
  as.data.frame 
predicciones_general_t<- cbind(x_categoricas_t,x_continuas_t)
predicciones_general_t<-predicciones_general_t[,-1]
names(predicciones_general_t)[names(predicciones_general_t)=='`DominioRESTO URBANO`']<-'DominioRESTO URBANO'
names(predicciones_general_t)[names(predicciones_general_t)=='`DominioSANTA MARTA`']<-"DominioSANTA MARTA"
predicciones_general_t<-as.matrix(predicciones_general_t)
predicciones_general<-as.matrix(predicciones_general)

#----------Problema de clasificaciòn
y_hat_ols_insample<-predict(ols$finalModel,predicciones_general)
y_hat_ols_outsample<-predict(ols$finalModel,predicciones_general_t)

y_hat_ols_insample1 <- as.numeric(ifelse(exp(y_hat_ols_insample)<train$Lp,1,0))
y_hat_ols_outsample1 <- as.numeric(ifelse(exp(y_hat_ols_outsample)<test$Lp,1,0))

#-----------Métricas para matriz 
acc_insample1 <- Accuracy(y_pred = y_hat_ols_insample1, y_true = train$Pobre)
acc_outsample1 <- Accuracy(y_pred = y_hat_ols_outsample1, y_true = test$Pobre)

pre_insample1 <- Precision(y_pred = y_hat_ols_insample1, y_true = train$Pobre, positive = 1)
pre_outsample1 <- Precision(y_pred = y_hat_ols_outsample1, y_true = test$Pobre, positive = 1)

rec_insample1 <- Recall(y_pred = y_hat_ols_insample1, y_true = train$Pobre, positive = 1)
rec_outsample1 <- Recall(y_pred = y_hat_ols_outsample1, y_true = test$Pobre, positive = 1)

f1_insample1 <- F1_Score(y_pred = y_hat_ols_insample1, y_true = train$Pobre, positive = 1)
f1_outsample1 <- F1_Score(y_pred = y_hat_ols_outsample1, y_true = test$Pobre, positive = 1)

metricas_insample1 <- data.frame(Modelo = "Regresión lineal", 
                                 "Muestreo" = NA, 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample1,
                                 "Precision" = pre_insample1,
                                 "Recall" = rec_insample1,
                                 "F1" = f1_insample1)

metricas_outsample1 <- data.frame(Modelo = "Regresión lineal", 
                                  "Muestreo" = NA, 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample1,
                                  "Precision" = pre_outsample1,
                                  "Recall" = rec_outsample1,
                                  "F1" = f1_outsample1)

metricas1 <- bind_rows(metricas_insample1, metricas_outsample1)
metricas1 

Modelo Muestreo        Evaluación  Accuracy Precision    Recall       F1
1 Regresión lineal       NA Dentro de muestra 0.8393020 0.6065061 0.5583397 0.581427
2 Regresión lineal       NA  Fuera de muestra 0.8389306 0.6091237 0.5586996 0.582823

#------------Gráfico de coeficientes OLS 

df_coeficientes_reg2 <- ols$finalModel$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes_reg2[-1,] %>%
  filter(predictor != "`(Intercept)`") %>%
  ggplot(aes(x = reorder(predictor, abs(coeficiente)), 
             y = coeficiente)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Coeficientes del modelo de regresión", 
       x = "Variables",
       y = "Coeficientes") +
  theme_bw()

#------------------------------------------------------ L A S S O -------------------------------------------------------
#-----------Base con x's de train

# Matrices de entrenamiento y test
# ==============================================================================
x_train <- predicciones_general
y_train <- train$log_ing_per

x_test <- predicciones_general_t
y_test <- test$log_ing_per


# Creación y entrenamiento del modelo
# ==============================================================================
# Para obtener un ajuste con regularización Lasso se indica argumento alpha=1.
# Si no se especifica valor de lambda, se selecciona un rango automático.
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 300,
  standardize = TRUE
)



# Evolución de los coeficientes en función de lambda
# ==============================================================================
regularizacion <- modelo$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

set.seed(123)

predicciones_general<-as.matrix(predicciones_general)
cv_error <- cv.glmnet(
  x      = predicciones_general,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)

# Mejor valor lambda encontrado
# ==============================================================================
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)

# Mejor valor lambda encontrado + 1sd
# ==============================================================================
# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)

# Mejor modelo lambda óptimo + 1sd
# ==============================================================================
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train <- predict(modelo, newx = predicciones_general)

# MSE de entrenamiento
# ==============================================================================
training_mse <- mean((predicciones_train - y_train)^2)
paste("Error (mse) de entrenamiento:", training_mse)

# Predicciones de test
# ==============================================================================
predicciones_general_t<-as.matrix(predicciones_general_t)
predicciones_test <- predict(modelo, newx = predicciones_general_t)

# MSE de test
# ==============================================================================
test_mse_lasso <- mean((predicciones_test - y_test)^2)
paste("Error (mse) de test:", test_mse_lasso)


r22 <- R2_Score(y_pred = predicciones_test, y_true = test$log_ing_per)
rmse2 <- RMSE(y_pred = predicciones_test, y_true = test$log_ing_per)
mae(test$log_ing_per, predicciones_test)   

is.numeric(predicciones_test)
#----------Problema de clasificaciòn
y_hat_lasso_insample<-predicciones_train
y_hat_lasso_outsample<-predicciones_test

y_hat_lasso_insample1 <- as.numeric(ifelse(exp(y_hat_lasso_insample)<train$Lp,1,0))
y_hat_lasso_outsample1 <- as.numeric(ifelse(exp(y_hat_lasso_outsample)<test$Lp,1,0))

#-----------Métricas para matriz 
acc_insample111 <- Accuracy(y_pred = y_hat_lasso_insample1, y_true = train$Pobre)
acc_outsample111 <- Accuracy(y_pred = y_hat_lasso_outsample1, y_true = test$Pobre)

pre_insample111 <- Precision(y_pred = y_hat_lasso_insample1, y_true = train$Pobre, positive = 1)
pre_outsample111 <- Precision(y_pred = y_hat_lasso_outsample1, y_true = test$Pobre, positive = 1)

rec_insample111 <- Recall(y_pred = y_hat_lasso_insample1, y_true = train$Pobre, positive = 1)
rec_outsample111 <- Recall(y_pred = y_hat_lasso_outsample1, y_true = test$Pobre, positive = 1)

f1_insample111 <- F1_Score(y_pred = y_hat_lasso_insample1, y_true = train$Pobre, positive = 1)
f1_outsample111 <- F1_Score(y_pred = y_hat_lasso_outsample1, y_true = test$Pobre, positive = 1)

metricas_insample111 <- data.frame(Modelo = "Regresión lineal", 
                                   "Muestreo" = NA, 
                                   "Evaluación" = "Dentro de muestra",
                                   "Accuracy" = acc_insample111,
                                   "Precision" = pre_insample111,
                                   "Recall" = rec_insample111,
                                   "F1" = f1_insample111)

metricas_outsample111 <- data.frame(Modelo = "Regresión lineal", 
                                    "Muestreo" = NA, 
                                    "Evaluación" = "Fuera de muestra",
                                    "Accuracy" = acc_outsample111,
                                    "Precision" = pre_outsample111,
                                    "Recall" = rec_outsample111,
                                    "F1" = f1_outsample111)

metricas111 <- bind_rows(metricas_insample111, metricas_outsample111)
metricas111 

Modelo Muestreo        Evaluación  Accuracy Precision    Recall        F1
1 Regresión lineal       NA Dentro de muestra 0.8388321 0.6342809 0.4575815 0.5316333
2 Regresión lineal       NA  Fuera de muestra 0.8381426 0.6361738 0.4584588 0.5328901

table(test$Pobre,y_hat_lasso_outsample1)
0     1
0 24606  1742
1  3598  3046
#----------------------------------------------------- R I D G E ------------------------------------------------------
# Matrices de entrenamiento y test
# =============================================================================
# Creación y entrenamiento del modelo
# ==============================================================================
# Para obtener un ajuste con regularización Ridge se indica argumento alpha=0.
# Si no se especifica valor de lambda, se selecciona un rango automático.
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  nlambda     = 500,
  standardize = TRUE
)

# Evolución de los coeficientes en función de lambda
# ==============================================================================
regularizacion <- modelo$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

# Mejor valor lambda encontrado
# ==============================================================================
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train <- predict(modelo, newx = predicciones_general)
# MSE de entrenamiento
# ==============================================================================
training_mse <- mean((predicciones_train - y_train)^2)
paste("Error (mse) de entrenamiento:", training_mse)

r22 <- R2_Score(y_pred = predicciones_test, y_true = test$log_ing_per)
rmse2 <- RMSE(y_pred = predicciones_test, y_true = test$log_ing_per)
mae(test$log_ing_per, predicciones_test)   


# Predicciones de test
# ==============================================================================
predicciones_test <- predict(modelo, newx = predicciones_general_t)
# MSE de test
# ==============================================================================
test_mse_ridge <- mean((predicciones_test - y_test)^2)
paste("Error (mse) de test:", test_mse_ridge)

set.seed(123)
cv_error <- cv.glmnet(
  x      = predicciones_general,
  y      =y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
) 

#Rigde ganador 
plot(cv_error)
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

#----------Problema de clasificaciòn
y_hat_r_insample<-predicciones_train
y_hat_r_outsample<-predicciones_test

y_hat_r_insample1 <- as.numeric(ifelse(exp(y_hat_r_insample)<train$Lp,1,0))
y_hat_r_outsample1 <- as.numeric(ifelse(exp(y_hat_r_outsample)<test$Lp,1,0))

#-----------Métricas para matriz 
acc_insample112 <- Accuracy(y_pred = y_hat_r_insample1, y_true = train$Pobre)
acc_outsample112 <- Accuracy(y_pred = y_hat_r_outsample1, y_true = test$Pobre)

pre_insample112 <- Precision(y_pred = y_hat_r_insample1, y_true = train$Pobre, positive = 1)
pre_outsample112 <- Precision(y_pred = y_hat_r_outsample1, y_true = test$Pobre, positive = 1)

rec_insample112 <- Recall(y_pred = y_hat_r_insample1, y_true = train$Pobre, positive = 1)
rec_outsample112 <- Recall(y_pred = y_hat_r_outsample1, y_true = test$Pobre, positive = 1)

f1_insample112 <- F1_Score(y_pred = y_hat_r_insample1, y_true = train$Pobre, positive = 1)
f1_outsample112 <- F1_Score(y_pred = y_hat_r_outsample1, y_true = test$Pobre, positive = 1)

metricas_insample112 <- data.frame(Modelo = "Regresión lineal", 
                                   "Muestreo" = NA, 
                                   "Evaluación" = "Dentro de muestra",
                                   "Accuracy" = acc_insample112,
                                   "Precision" = pre_insample112,
                                   "Recall" = rec_insample112,
                                   "F1" = f1_insample112)

metricas_outsample112 <- data.frame(Modelo = "Regresión lineal", 
                                    "Muestreo" = NA, 
                                    "Evaluación" = "Fuera de muestra",
                                    "Accuracy" = acc_outsample112,
                                    "Precision" = pre_outsample112,
                                    "Recall" = rec_outsample112,
                                    "F1" = f1_outsample112)

metricas112 <- bind_rows(metricas_insample112, metricas_outsample112)
metricas112

Modelo Muestreo        Evaluación  Accuracy Precision    Recall        F1
1 Regresión lineal       NA Dentro de muestra 0.8421208 0.6706259 0.4130781 0.5112482
2 Regresión lineal       NA  Fuera de muestra 0.8414464 0.6704463 0.4182721 0.5151543

#-------------------------------------------------E L A S T I C   N E T --------------------------------------------------            
# Model Building : Elastic Net Regression
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = TRUE)

y_train2<-upSampledTrain$log_ing_per
set.seed(12345)
en <- train(y_train~.,
            data=cbind(x_train2,y_train2),
            method='glmnet',
            tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                  lambda = seq(0.0001,0.2,length=5)),
            trControl=custom)
#Resultados 
"mean(en$resample$RMSE) 1.036258
             MSE was used to select the optimal model using the smallest value.
             The final values used for the model were alpha = 0.6666667 and lambda = 1e-04      "        
r22 <- R2_Score(y_pred = predicciones_test, y_true = test$log_ing_per)
rmse2 <- RMSE(y_pred = predicciones_test, y_true = test$log_ing_per)
mae(test$log_ing_per, predicciones_test)   

#Ploting EN
plot(en, main = "Elastic Net Regression")
#plotting important variables
plot(varImp(en,scale=TRUE))

#----------Problema de clasificaciòn
modelo<- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0.6666667,
  lambda      = 0.0001,
  standardize = TRUE
)

predicciones_train <- predict(modelo, newx = predicciones_general)
predicciones_test <- predict(modelo, newx = predicciones_general_t)

y_hat_en_insample<-predicciones_train
y_hat_en_outsample<-predicciones_test

y_hat_en_insample1 <- as.numeric(ifelse(exp(y_hat_en_insample)<train$Lp,1,0))
y_hat_en_outsample1 <- as.numeric(ifelse(exp(y_hat_en_outsample)<test$Lp,1,0))

#-----------Métricas para matriz 
acc_insample1122 <- Accuracy(y_pred = y_hat_en_insample1, y_true = train$Pobre)
acc_outsample1122 <- Accuracy(y_pred = y_hat_en_outsample1, y_true = test$Pobre)

pre_insample1122 <- Precision(y_pred = y_hat_en_insample1, y_true = train$Pobre, positive = 1)
pre_outsample1122 <- Precision(y_pred = y_hat_en_outsample1, y_true = test$Pobre, positive = 1)

rec_insample1122<- Recall(y_pred = y_hat_en_insample1, y_true = train$Pobre, positive = 1)
rec_outsample1122 <- Recall(y_pred = y_hat_en_outsample1, y_true = test$Pobre, positive = 1)

f1_insample1122 <- F1_Score(y_pred = y_hat_en_insample1, y_true = train$Pobre, positive = 1)
f1_outsample1122 <- F1_Score(y_pred = y_hat_en_outsample1, y_true = test$Pobre, positive = 1)

metricas_insample1122 <- data.frame(Modelo = "Regresión lineal", 
                                    "Muestreo" = NA, 
                                    "Evaluación" = "Dentro de muestra",
                                    "Accuracy" = acc_insample1122,
                                    "Precision" = pre_insample1122,
                                    "Recall" = rec_insample1122,
                                    "F1" = f1_insample1122)

metricas_outsample1122 <- data.frame(Modelo = "Regresión lineal", 
                                     "Muestreo" = NA, 
                                     "Evaluación" = "Fuera de muestra",
                                     "Accuracy" = acc_outsample1122,
                                     "Precision" = pre_outsample1122,
                                     "Recall" = rec_outsample1122,
                                     "F1" = f1_outsample1122)

metricas1122 <- bind_rows(metricas_insample112, metricas_outsample112)
metricas1122

Modelo Muestreo        Evaluación  Accuracy Precision    Recall        F1
1 Regresión lineal       NA Dentro de muestra 0.8421208 0.6706259 0.4130781 0.5112482
2 Regresión lineal       NA  Fuera de muestra 0.8414464 0.6704463 0.4182721 0.5151543

## En
en_prob = confusionMatrix(data=factor(y_hat_outsample1) , 
                          reference=factor(test$Pobre) , 
                          mode="sens_spec" , positive="1")
en_prob
onfusion Matrix and Statistics

Reference
Prediction     0     1
0 24915  5995
1  1433   649

Accuracy : 0.7749          
95% CI : (0.7703, 0.7794)
No Information Rate : 0.7986          
P-Value [Acc > NIR] : 1               

Kappa : 0.0583          

Mcnemars Test P-Value : <2e-16          

Sensitivity : 0.09768         
Specificity : 0.94561         
Pos Pred Value : 0.31172         
Neg Pred Value : 0.80605         
Prevalence : 0.20138         
Detection Rate : 0.01967         
Detection Prevalence : 0.06311         
Balanced Accuracy : 0.52165         

'Positive' Class : 1 
#----------------------------------- B a l a n c e o   d e   l a   m u e s t r a ---------------------------------

x_train<-cbind(x_train,train$log_ing_per)
x_train<-cbind(x_train, train$Pobre)

train$Pobre<-as.factor(train$Pobre)


#---------------------- Oversamplig 
set.seed(1103)
upSampledTrain <- upSample(x = train,
                           y = train$Pobre,
                           ## keep the class variable name the same:
                           yname = "Default")
train$Pobre<-as.factor(train$Pobre)

names(upSampledTrain)[names(upSampledTrain)=='train$log_ing_per']<-"log_ing_per"
names(upSampledTrain)[names(upSampledTrain)=='train$Pobre']<-"Pobre"

#-------------------------------------------------E L A S T I C   N E T --------------------------------------------------            
#-------------------------------------------------- B A L A N C E A D O ---------------------------------------------

# Model Building : Elastic Net Regression
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = TRUE)
y_train=upSampledTrain$log_ing_per
x_train<- upSampledTrain[,-61]       
x_train<- x_train[,-60]
x_train

set.seed(12345)
en <- train(y_train~.,
            data=cbind(y_train,x_train),
            method='glmnet',
            tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                  lambda = seq(0.0001,0.2,length=5)),
            trControl=custom)
#Resultados 
"mean(en$resample$RMSE) 1.036258
             MSE was used to select the optimal model using the smallest value.
             The final values used for the model were alpha = 0.6666667 and lambda = 1e-04      "        
r22 <- R2_Score(y_pred = predicciones_test, y_true = test$log_ing_per)
rmse2 <- RMSE(y_pred = predicciones_test, y_true = test$log_ing_per)
mae(test$log_ing_per, predicciones_test)   

#Ploting EN
plot(en, main = "Elastic Net Regression")
#plotting important variables
plot(varImp(en,scale=TRUE))


#----------Problema de clasificaciòn
modelo<- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0.444,
  lambda      = 0.0001,
  standardize = TRUE
)
modelo$beta

predicciones_train <- predict(modelo, newx = x_train)
x_train<-as.matrix(x_train)
predicciones_test <- predict(modelo, newx = predicciones_general_t)

y_hat_en_insample<-predicciones_train
y_hat_en_outsample<-predicciones_test

y_hat_en_insample1 <- as.numeric(ifelse(exp(y_hat_en_insample)<train$Lp,1,0))
y_hat_en_outsample1 <- as.numeric(ifelse(exp(y_hat_en_outsample)<test$Lp,1,0))

#-----------Métricas para matriz 
acc_insample1122 <- Accuracy(y_pred = y_hat_en_insample1, y_true = train$Pobre)
acc_outsample1122 <- Accuracy(y_pred = y_hat_en_outsample1, y_true = test$Pobre)

pre_insample1122 <- Precision(y_pred = y_hat_en_insample1, y_true = train$Pobre, positive = 1)
pre_outsample1122 <- Precision(y_pred = y_hat_en_outsample1, y_true = test$Pobre, positive = 1)

rec_insample1122<- Recall(y_pred = y_hat_en_insample1, y_true = train$Pobre, positive = 1)
rec_outsample1122 <- Recall(y_pred = y_hat_en_outsample1, y_true = test$Pobre, positive = 1)

f1_insample1122 <- F1_Score(y_pred = y_hat_en_insample1, y_true = train$Pobre, positive = 1)
f1_outsample1122 <- F1_Score(y_pred = y_hat_en_outsample1, y_true = test$Pobre, positive = 1)

metricas_insample1122 <- data.frame(Modelo = "Regresión lineal", 
                                    "Muestreo" = NA, 
                                    "Evaluación" = "Dentro de muestra",
                                    "Accuracy" = acc_insample1122,
                                    "Precision" = pre_insample1122,
                                    "Recall" = rec_insample1122,
                                    "F1" = f1_insample1122)

metricas_outsample1122 <- data.frame(Modelo = "Regresión lineal", 
                                     "Muestreo" = NA, 
                                     "Evaluación" = "Fuera de muestra",
                                     "Accuracy" = acc_outsample1122,
                                     "Precision" = pre_outsample1122,
                                     "Recall" = rec_outsample1122,
                                     "F1" = f1_outsample1122)

metricas1122 <- bind_rows(metricas_insample112, metricas_outsample112)
metricas1122

Modelo Muestreo        Evaluación  Accuracy Precision    Recall        F1
1 Regresión lineal       NA Dentro de muestra 0.8421208 0.6706259 0.4130781 0.5112482
2 Regresión lineal       NA  Fuera de muestra 0.8414464 0.6704463 0.4182721 0.5151543

## En
en_prob = confusionMatrix(data=factor(y_hat_en_outsample1) , 
                          reference=factor(test$Pobre) , 
                          mode="sens_spec" , positive="1")
en_prob
onfusion Matrix and Statistics

Reference
Prediction     0     1
0 24915  5995
1  1433   649

Accuracy : 0.7749          
95% CI : (0.7703, 0.7794)
No Information Rate : 0.7986          
P-Value [Acc > NIR] : 1               

Kappa : 0.0583          

Mcnemars Test P-Value : <2e-16          

Sensitivity : 0.09768         
Specificity : 0.94561         
Pos Pred Value : 0.31172         
Neg Pred Value : 0.80605         
Prevalence : 0.20138         
Detection Rate : 0.01967         
Detection Prevalence : 0.06311         
Balanced Accuracy : 0.52165         

'Positive' Class : 1 


#-------------------------------------------------L o g i t  y  P r o b i t----------------------------------------------------------------------------
## modelo a ajustar
model <- as.formula("train$Pobre ~ .")
as.matrix(cx)
cx<- as.data.frame(cx)
## estimación logit
logit <- glm(model , family=binomial(link="logit") , data=cx)
tidy(logit)

## estimación probit
probit <- glm(model , family=binomial(link="probit") , data=cx)
tidy(probit)

## ratio de los coeficientes
logit$coefficients / probit$coefficients
## preddicción
train$pob_log = predict(logit , newdata=cx , type="response")
train$pob_pob = predict(probit , newdata=cx , type="response")
head(train)

## plot predictions
ggplot(data=train, mapping=aes(Pobre,pob_pob)) + 
  geom_boxplot(aes(fill=as.factor(Pobre))) + theme_test()

## definir la regla
rule=0.35
train$prob_025 = ifelse(train$pob_pob<rule,1,0)
train$log_025 = ifelse(train$pob_log<rule,1,0)
head(train)

table(train$Pobre, train$prob_025)
0     1
0 19328 86260
1 20469  5911

#------ Cutoff óptimo     

evalResults <- data.frame(Default = evaluation$Default)
evalResults$Roc <- predict(logit,
                           newdata = test,
                           type = "prob")[,1]
#-----------Cladificaciòn
## probit
cm_prob = confusionMatrix(data=factor(test$prob_025) , 
                          reference=factor(test$Pobre) , 
                          mode="sens_spec" , positive="1")
cm_prob

Reference
Prediction     0     1
0 19328 20469
1 86260  5911


#---------------------------------------- T r e e s ---------------------------------

#--------------------------------- R a n d o m   F o r e s t  ---------------------------------
install.packages("ranger")
library(ranger)
y_train2<-upSampledTrain$Pobre
x_train2<-upSampledTrain
x_train2<-x_train2[,-62]
x_train2<-x_train2[,-61]
x_train2<-x_train2[,-60]

# Creamos una grilla para tunear el random forest
set.seed(12345)
cv3 <- trainControl(number = 3, method = "cv")
tunegrid_rf <- expand.grid(mtry = c(3, 5, 10), 
                           min.node.size = c(10, 30, 50,
                                             70, 100),
                           splitrule="gini"
)

modeloRF <- train(y_train2 ~ .,
                  data = cbind(y_train2, x_train2), 
                  method = "ranger", 
                  trControl = cv3,
                  metric = 'Recall', 
                  verbose = TRUE,
                  tuneGrid = tunegrid_rf)


#------ Comando manual
ggplot(modeloRF$results, aes(x = min.node.size, y = Accuracy, 
                             color = as.factor(mtry))) +
  geom_line() +
  geom_point() +
  labs(title = "Resultados del grid search",
       x = "Mínima cantidad de observaciones por hoja",
       y = "Acuraccy (Cross-Validation)") +
  scale_color_discrete("Número de predictores seleccionados al azar") +
  theme_bw() +
  theme(legend.position = "bottom")

#----- El mejor modelo es aquel que tiene mtry = X y min.node.size = X
y_hat_insample2 = predict(modeloRF, newdata = x_train2)
y_hat_outsample2 = predict(modeloRF, newdata = predicciones_general_t)
modeloRF$coefnames

#-----metricas 
acc_insample2 <- Accuracy(y_pred = y_hat_insample2, y_true = y_train)
acc_outsample2 <- Accuracy(y_pred = y_hat_outsample2, y_true = test$Pobre)

pre_insample2 <- Precision(y_pred = y_hat_insample2, y_true = y_train, positive = 1)
pre_outsample2 <- Precision(y_pred = y_hat_outsample2, y_true = test$Pobre, positive = 1)

rec_insample2 <- Recall(y_pred = y_hat_insample2, y_true = y_train, positive = 1)
rec_outsample2 <- Recall(y_pred = y_hat_outsample2, y_true = test$Pobre, positive = 1)

f1_insample2 <- F1_Score(y_pred = y_hat_insample2, y_true = y_train, positive = 1)
f1_outsample2 <- F1_Score(y_pred = y_hat_outsample2, y_true = test$Pobre, positive = 1)

metricas_insample2 <- data.frame(Modelo = "Random Forest", 
                                 "Muestreo" = NA, 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample1,
                                 "Precision" = pre_insample1,
                                 "Recall" = rec_insample1,
                                 "F1" = f1_insample1)

metricas_outsample2 <- data.frame(Modelo = "Random Forest", 
                                  "Muestreo" = NA, 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample1,
                                  "Precision" = pre_outsample1,
                                  "Recall" = rec_outsample1,
                                  "F1" = f1_outsample1)

metricas2 <- bind_rows(metricas_insample2, metricas_outsample2)
metricas2 

------# Importancia de las variables
  
  modeloRF$results
importancia <- varImp(modeloRF$)

varImpPlot(modeloRF)

importancia <- importancia %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))

ggplot(importancia, aes(x = Porcentaje, 
                        y = reorder(Variable, Porcentaje))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  labs(y = "Variable") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic()
#------------------------------------------- P r e d i c c i ó n  m u e s t r a--------------------------------------------
#-------------------------------------------------------F i n a l ------------------------------------------------------------

