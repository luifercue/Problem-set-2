#-------------------------------------------------------PROBLEM SET 2 ------------------------------------------

#-------------------------------------------------Lectura bases ------------------------------------------------------------------------------------
    train_hogares<-readRDS(here("../data/train_hogares.Rds"))
    train_personas<-readRDS(here("../data/train_personas.Rds"))

#-------Unión de variables 
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
          
            #----------------------------------- E s t a d í s t i c a s   D e s c r i p t i v a s ---------------------------------
            library(readxl)
            install.packages("naniar")
            library(naniar)
            library(ggplot2)
            library(dplyr)
            install.packages('simputation')
            library(simputation)
            library(visdat)
            install.packages("Hmisc")
            library(Hmisc)
            
            table (train_hogares$Pobre) #131936  33024
            table(train_hogares$Dominio)
            
            #----------------Características de la Vivienda---------
            
            table(train_hogares$Tipo_vivienda)
            
            prop.test(table(train_hogares$Tipo_vivienda,train_hogares$Pobre))
            
            prop.table(table(train_hogares$Pobre,train_hogares$Tipo_vivienda), margin=2)
            
            table(train_hogares$Pobre)
            
            tabla2<-table(train_hogares$Tipo_vivienda,train_hogares$Ncuartos_dormir)
            tabla2
            
            
            train_hogares %>%
              group_by(Pobre) %>%
              summarise_at(.vars = "Ncuartos",
                           .funs = c("mean", "sd", "var", "min", "max"),
                           na.rm = TRUE)
            
            
            train_hogares %>%
              group_by(Pobre) %>%
              summarise_at(.vars = "hacinamiento",
                           .funs = c("mean", "sd", "var", "min", "max"),
                           na.rm = TRUE)
            
            
            
            #--------------------------Dominio | Pobre-------------------------
            train_hogares %>%
              group_by(Dominio) %>%
              summarise_at(.vars = "Tipo_vivienda",
                           .funs = c("mean", "sd", "var", "min", "max"),
                           na.rm = TRUE)
            
            dh1 <- train_hogares %>%
              group_by(Dominio) %>%
              summarise_at(.vars = "hacinamiento",
                           .funs = c("mean", "sd", "var", "min", "max"),
                           na.rm = TRUE)
            print(dh1,n=25)
            #--------------------------Sexo Jefe del Hogar | Ingcug-------------------------
            
            
            describe(train_hogares$sexo_jefe_hogar)
            
            prop.table(table(train_hogares$sexo_jefe_hogar))
            
            
            ggplot(train_hogares, aes(x = sexo_jefe_hogar)) +
              geom_bar(fill = "darkblue") +
              theme_bw() +
              labs(title = " ¿Cual es el género del jefe del hogar? 
                        si=1, no=0",
                   x = "",
                   y = "Frecuencia") +
              coord_flip()
            
            table(train_personas$Dominio,train_personas$sexo_jefe_hogar)
            
            train_hogares %>%
              group_by(sexo_jefe_hogar) %>%
              summarise_at(.vars = "Ingpcug",
                           .funs = c("mean", "sd", "var", "min", "max"),
                           na.rm = TRUE)
            
            da1 <-train_hogares %>%
              group_by(Dominio) %>%
              summarise_at(.vars = "Ingpcug",
                           .funs = c("mean", "sd", "var", "min", "max"),
                           na.rm = TRUE)
            print(da1,n=25)
            
            prop.table(table(train_hogares$sexo_jefe_hogar, train_hogares$Dominio), margin=2)
            
            #--------------------------Subsidios Pobreza Dominio-------------------------
            
            
            train_hogares %>%
              group_by(Pobre) %>%
              summarise_at(.vars = "Horas_trabajo1",
                           .funs = c("mean", "sd", "var", "min", "max"),
                           na.rm = T)
            
            train_hogares %>%
              group_by(Pobre) %>%
              summarise_at(.vars = "edad_jefe_hogar",
                           .funs = c("mean", "sd", "var", "min", "max"),
                           na.rm = T)
            
            summarise(train_hogares$edad_jefe_hogar)
            
            summary(train_hogares$edad_jefe_hogar)
            
            prop.table(table(train_hogares$sexo_jefe_hogar, train_hogares$ocupacion_jefe_hogar))
            
            
            prop.table(table(train_hogares$ocupacion_jefe_hogar, train_hogares$subsidio))
            
            #---------------------------- Gráfica Pobreza por Ingreso-----------------------
            
            library(tidyverse)
            library(ggplot2)
            install.packages("vtable")
            library(vtable)
            library(kableExtra)
            install.packages("gtsummary")
            library(gtsummary)
            
            
            ggplot(data = train_hogares , mapping = aes(x = id, y = Ingpcug, group=as.factor(Pobre) , color=as.factor(Pobre) )) + 
              geom_jitter(width = 0.005, height = 0.1)+ geom_smooth(span = 0.3)  + ylim(0, 80000000) + scale_y_continuous(trans = "log10") +
              scale_fill_manual(values = c("0"="red" , "1"="blue") , label = c("0"="No Pobre" , "1"="Pobre") , name = "Clasificación Pobreza") +
              labs (title= "Ingresos Per Capita por Clasificación del Hogar Pobre vs No Pobre", 
                    x="Clasificacion Pobreza | 0 = No Pobre    1 = Pobre", 
                    y="Ingreso Hogar", 
                    caption= " Datos GEIH 2O18") + theme_classic()
            
            
            
            #-------------Tablas Variables Categoricas--------------------------------------------------------
            
            categorical_table <- train_hogares %>% select(Dominio, Pobre, sexo_jefe_hogar, subsidio) %>% 
              tbl_summary(type = list(Dominio~ "categorical", Pobre~ "categorical", sexo_jefe_hogar~ "categorical", subsidio~ "categorical"), missing="no") 
            
            
            
            categorical_tablee <- train_hogares %>% select(Pobre,sexo_jefe_hogar,rs_jefe_hogar,edu_jefe_hogar,ocupacion_jefe_hogar) %>% 
              tbl_summary(type = list(Pobre~ "categorical", sexo_jefe_hogar~ "categorical", rs_jefe_hogar~ "categorical", edu_jefe_hogar~ "categorical",ocupacion_jefe_hogar~ "categorical"), missing="no") 
            
            categorical_tablee
            
            
            #-------------------------------Tabla Variables Continuas-----------------------------------------------------------------
            
            st(train_hogares, vars=c('Ncuartos', 'Nper', 'Ingpcug', 'Lp', 'hacinamiento', 'edad_jefe_hogar','Horas_trabajo1', 'Horas_trabajo2', 'arriendo_estimado'), 
               labels=c('Numero de Cuartos - Incluye Sala', 'Numero de Personas por Hogar', 
                        'Ingresos (Pesos)', 'Linea de Pobreza','Hacinamiento',
                        'Edad del Jefe del Hogar', 'Horas Trabajadas Hogar (Semanales)','Horas Trabajadas Hogar Segundo Empleo (Semanales)',
                        'Arriendo (Pesos)'), summ.names = list(c('Observaciones','Promedio','Desv.Est.','Min','Max')))
            
            
            
            
            
#----------------------------------- D i v i s i ó n  d e  M u e s t r a ---------------------------------
            set.seed(12345) 
            train_hogares2 <- train_hogares2 %>%
              mutate(holdout= as.logical(1:nrow(train_hogares2) %in%
                                           sample(nrow(train_hogares2), nrow(train_hogares2)*.2)))
            test<-train_hogares2[train_hogares2$holdout==T,] #32.992
            train<-train_hogares2[train_hogares2$holdout==F,] #131.968
            
#----------------------------------- M o d e l o s   d e  e s t i m a c i ó n ----------------------------------------------------------------------------- 
#------------------------------------------------------- OLS--------------------------------------------------------------------------------------- 
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
            x_continuas=(train[,c(3,4,14,16,21,22,23)])
            x_categoricas=(train[,c(2,5,6,7,15,17,18,19,20)])
            # Ahora procedemos a dummyficar la base
            x_categoricas<- model.matrix(~ ., x_categoricas) %>%
              as.data.frame #%>%
            predicciones_general<- cbind(x_categoricas,x_continuas)
            predicciones_general<-predicciones_general[,-1]
            
            #test
            x_continuas_t=(test[,c(3,4,14,16,21,22,23)])
            x_categoricas_t=(test[,c(2,5,6,7,15,17,18,19,20)])
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
            set.seed(12345)
            en <- train(y_train~.,
                        data=cbind(x_train,y_train),
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
       train$Pobre<-as.factor(train$Pobre)
       
       
       #---------------------- Oversamplig 
       set.seed(1103)
       upSampledTrain <- upSample(x = x_train,
                                  y = train$Pobre,
                                  ## keep the class variable name the same:
                                  yname = "Default")
       
       
       names(upSampledTrain)[names(upSampledTrain)=='train$log_ing_per']<-"log_ing_per"
       
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
       
 
#-------------------------------------------------------------------------------------------------------------------------------------
#                                   M o d e l o s  d e  C l a s i f i c a c i ó n
#------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------L o g i t  y  P r o b i t----------------------------------------------------------------------------
        ## modelo a ajustar
        model <- as.formula("train$Pobre ~ .")
        as.matrix(predicciones_general)
        cx<- as.data.frame(predicciones_general_t)
        as.matrix(predicciones_general)
        cxt<- as.data.frame(predicciones_general_t)
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
        
        test$pob_log = predict(logit , newdata=cxt , type="response")
        test$pob_pob = predict(probit , newdata=cxt , type="response")
        ## plot predictions
        ggplot(data=train, mapping=aes(Pobre,pob_pob)) + 
        geom_boxplot(aes(fill=as.factor(Pobre))) + theme_test()
        
        ## definir la regla
        rule=0.35
        train$prob_025 = ifelse(train$pob_pob<rule,1,0)
        train$log_025 = ifelse(train$pob_log<rule,1,0)
        head(train)
        
        test$prob_025 = ifelse(test$pob_pob<rule,1,0)
        test$log_025 = ifelse(test$pob_log<rule,1,0)
        
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
        cm_prob_in = confusionMatrix(data=factor(train$prob_025) , 
                                  reference=factor(train$Pobre) , 
                                  mode="sens_spec" , positive="1")
        cm_prob_out =confusionMatrix(data=factor(test$prob_025) , 
                                     reference=factor(test$Pobre) , 
                                     mode="sens_spec" , positive="1")
        
        cm_prob_in = confusionMatrix(data=factor(train$log_025) , 
                                     reference=factor(train$Pobre) , 
                                     mode="sens_spec" , positive="1")
        cm_prob_out =confusionMatrix(data=factor(test$log_025) , 
                                     reference=factor(test$Pobre) , 
                                     mode="sens_spec" , positive="1")
        
                      Reference
        Prediction     0     1
                    0 19328 20469
                    1 86260  5911
        
        Accuracy : 0.1913          
        95% CI : (0.1891, 0.1934)
        No Information Rate : 0.8001          
        P-Value [Acc > NIR] : 1               
        
        Kappa : -0.3063         
        
        Mcnemars Test P-Value : <2e-16          
                                          
            Sensitivity : 0.22407         
            Specificity : 0.18305         
         Pos Pred Value : 0.06413         
         Neg Pred Value : 0.48566         
             Prevalence : 0.19990         
         Detection Rate : 0.04479         
   Detection Prevalence : 0.69843         
      Balanced Accuracy : 0.20356
      #metricas
      
      acc_in <- Accuracy(y_true = train$Pobre, y_pred = train$prob_025)
      acc_in <- round(100*acc_in, 2)
      pre_in <- Precision(y_true = train$Pobre, y_pred = train$prob_025)
      pre_in <- round(100*pre_in, 2)
      recall_in <- Recall(y_true = train$Pobre, y_pred = train$prob_025)
      recall_in <- round(100*recall_in, 2)
      f1_in <- F1_Score(y_true = train$Pobre, y_pred = train$prob_025)
      f1_in <- round(100*f1_in, 2)
      
      acc_out <- Accuracy(y_true = test$Pobre, y_pred = test$prob_025)
      acc_out <- round(100*acc_out, 2)
      pre_out <- Precision(y_true = test$Pobre, y_pred = test$prob_025)
      pre_out <- round(100*pre_out, 2)
      recall_out <- Recall(y_true = test$Pobre, y_pred = test$prob_025)
      recall_out <- round(100*recall_out, 2)
      f1_out <- F1_Score(y_true = test$Pobre, y_pred = test$prob_025)
      f1_out <- round(100*f1_out, 2)
      
      resultados <- data.frame(Modelo = "Modelo 2: Grid search", Base = c("Train", "Test"), 
                                Accuracy = c(acc_in, acc_out), 
                                Precision = c(pre_in, pre_out),
                                Recall = c(recall_in, recall_out),
                                F1 = c(f1_in, f1_out))
      
      resultados #probit
      
      #Modelo  Base Accuracy Precision Recall    F1
      #1 Modelo 2: Grid search Train    15.99     40.97  11.35 17.78
      #2 Modelo 2: Grid search  Test    16.01     40.73  11.37 17.77
      
      
      acc_in <- Accuracy(y_true = train$Pobre, y_pred = train$log_025)
      acc_in <- round(100*acc_in, 2)
      pre_in <- Precision(y_true = train$Pobre, y_pred = train$log_025)
      pre_in <- round(100*pre_in, 2)
      recall_in <- Recall(y_true = train$Pobre, y_pred = train$log_025)
      recall_in <- round(100*recall_in, 2)
      f1_in <- F1_Score(y_true = train$Pobre, y_pred = train$log_025)
      f1_in <- round(100*f1_in, 2)
      
      acc_out <- Accuracy(y_true = test$Pobre, y_pred = test$log_025)
      acc_out <- round(100*acc_out, 2)
      pre_out <- Precision(y_true = test$Pobre, y_pred = test$log_025)
      pre_out <- round(100*pre_out, 2)
      recall_out <- Recall(y_true = test$Pobre, y_pred = test$log_025)
      recall_out <- round(100*recall_out, 2)
      f1_out <- F1_Score(y_true = test$Pobre, y_pred = test$log_025)
      f1_out <- round(100*f1_out, 2)
      
      resultados <- data.frame(Modelo = "Modelo : Grid search", Base = c("Train", "Test"), 
                               Accuracy = c(acc_in, acc_out), 
                               Precision = c(pre_in, pre_out),
                               Recall = c(recall_in, recall_out),
                               F1 = c(f1_in, f1_out))
      
      resultados #logit
      
      
      #Modelo  Base Accuracy Precision Recall    F1
      #1 Modelo : Grid search Train    15.72     40.19  10.93 17.19
      #2 Modelo : Grid search  Test    15.73     39.94  10.98 17.22
#------------------------------------------------------T r e e s --------------------------------------------------------      
#-----------------------------arbol1------------------------------------------
      
      # Convertimos la marca a factor
      train$Pobre <- factor(train$Pobre)
      test$Pobre <- factor(test$Pobre)
      
      # Creamos modelo
      modelo2 <- decision_tree(
        cost_complexity = tune(),
        tree_depth = tune(),
        min_n = tune()
      ) %>%
        set_engine("rpart") %>%
        set_mode("classification")
      #creamos grilla
      tree_grid <- crossing(
        cost_complexity = c(0.0001),
        min_n = c(2, 14, 27),
        tree_depth = c(4, 8, 16)
      )
      #definimos CV
      
      set.seed(666)
      folds <- vfold_cv(train, strata = Pobre, v = 5)
      set.seed(666)
      modelo2_cv <- tune_grid(
        modelo2,
        Pobre ~ Dominio+Ncuartos+Ncuartos_dormir+Tipo_vivienda+Nper+Npersug+hacinamiento+sexo_jefe_hogar+
          edad_jefe_hogar+rs_jefe_hogar+edu_jefe_hogar+Horas_trabajo2+ocupacion_jefe_hogar+subsidio+Horas_trabajo1+
          +arriendo_estimado,
        resamples = folds,
        grid = tree_grid,
        metrics = metric_set(f_meas),
        control = control_grid(event_level = 'second')
      )
      
      collect_metrics(modelo2_cv)
      autoplot(modelo2_cv) + 
        theme_light() +
        labs(y = "F1 Score")
      
      # Escogemos el mejor modelo
      modelo2 <- finalize_model(modelo2, select_best(modelo2_cv))
      
      # Entrenamos el mejor modelo
      modelo2_fit <- fit(modelo2, Pobre ~ Dominio+Ncuartos+Ncuartos_dormir+Tipo_vivienda+Nper+Npersug+hacinamiento+sexo_jefe_hogar+
                           edad_jefe_hogar+rs_jefe_hogar+edu_jefe_hogar+Horas_trabajo2+ocupacion_jefe_hogar+subsidio+Horas_trabajo1+
                           +arriendo_estimado, train)
      
      # Gráfica del modelo
      fancyRpartPlot(modelo2_fit$fit, main = "Árbol con fine tuning", 
                     sub = "")
      
      # Importancia de las variables
      importancia <- varImp(modelo2_fit$fit)
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
      
      # Evaluamos
      
      y_hat_insample <- predict(modelo2_fit, train)$.pred_class
      y_hat_outsample <- predict(modelo2_fit, test)$.pred_class
      
      
      
      
      cm_insample<-confusionMatrix(data=factor(y_hat_insample) , 
                                   reference=factor(train$Pobre) , 
                                   mode="sens_spec" , positive="1")$table
      
      
      cm_outsample<-confusionMatrix(data=factor(y_hat_outsample) , 
                                    reference=factor(test$Pobre) , 
                                    mode="sens_spec" , positive="1")$table
      
      # Confusion Matrix insample
      cm_insample
      # Confusion Matrix outsample
      cm_outsample
      
      #metricas
      
      acc_in <- Accuracy(y_true = train$Pobre, y_pred = y_hat_insample)
      acc_in <- round(100*acc_in, 2)
      pre_in <- Precision(y_true = train$Pobre, y_pred = y_hat_insample)
      pre_in <- round(100*pre_in, 2)
      recall_in <- Recall(y_true = train$Pobre, y_pred = y_hat_insample)
      recall_in <- round(100*recall_in, 2)
      f1_in <- F1_Score(y_true = train$Pobre, y_pred = y_hat_insample)
      f1_in <- round(100*f1_in, 2)
      
      acc_out <- Accuracy(y_true = test$Pobre, y_pred = y_hat_outsample)
      acc_out <- round(100*acc_out, 2)
      pre_out <- Precision(y_true = test$Pobre, y_pred = y_hat_outsample)
      pre_out <- round(100*pre_out, 2)
      recall_out <- Recall(y_true = test$Pobre, y_pred = y_hat_outsample)
      recall_out <- round(100*recall_out, 2)
      f1_out <- F1_Score(y_true = test$Pobre, y_pred = y_hat_outsample)
      f1_out <- round(100*f1_out, 2)
      
      resultados2 <- data.frame(Modelo = "Modelo 2: Grid search", Base = c("Train", "Test"), 
                                Accuracy = c(acc_in, acc_out), 
                                Precision = c(pre_in, pre_out),
                                Recall = c(recall_in, recall_out),
                                F1 = c(f1_in, f1_out))
      
      resultados2
      
      #Modelo  Base Accuracy Precision Recall    F1
      #1 Modelo 2: Grid search Train    88.34     90.69  95.20 92.89
      #2 Modelo 2: Grid search  Test    85.21     88.78  93.27 90.97
      
      
#-----------------------------arbol2------------------------------------------
      
      
      # Convertimos la marca a factor
      train$Pobre <- factor(train$Pobre)
      test$Pobre <- factor(test$Pobre)
      
      # Creamos modelo
      modelo3 <- decision_tree(
        cost_complexity = tune(),
        tree_depth = tune(),
        min_n = tune()
      ) %>%
        set_engine("rpart") %>%
        set_mode("classification")
      #creamos grilla
      tree_grid <- crossing(
        cost_complexity = c(0.0001),
        min_n = c(2, 14, 27),
        tree_depth = c(4, 8, 16)
      )
      #definimos CV
      
      set.seed(666)
      folds <- vfold_cv(train, strata = Pobre, v = 5)
      set.seed(666)
      modelo3_cv <- tune_grid(
        modelo3,
        Pobre ~ Dominio+Nper+Npersug+hacinamiento
        +rs_jefe_hogar+ocupacion_jefe_hogar+subsidio+Horas_trabajo1+
          +arriendo_estimado,
        resamples = folds,
        grid = tree_grid,
        metrics = metric_set(f_meas),
        control = control_grid(event_level = 'second')
      )
      
      collect_metrics(modelo3_cv)
      autoplot(modelo3_cv) + 
        theme_light() +
        labs(y = "F1 Score")
      
      # Escogemos el mejor modelo
      modelo3 <- finalize_model(modelo3, select_best(modelo3_cv))
      
      # Entrenamos el mejor modelo
      modelo3_fit <- fit(modelo3, Pobre ~ Dominio+Nper+Npersug+hacinamiento
                         +rs_jefe_hogar+ocupacion_jefe_hogar+subsidio+Horas_trabajo1+
                           +arriendo_estimado, train)
      
      # Gráfica del modelo
      fancyRpartPlot(modelo2_fit$fit, main = "Árbol con fine tuning", 
                     sub = "")
      
      # Importancia de las variables
      importancia <- varImp(modelo3_fit$fit)
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
      
      # Evaluamos
      
      y_hat_insample <- predict(modelo3_fit, train)$.pred_class
      y_hat_outsample <- predict(modelo3_fit, test)$.pred_class
      
      
      cm_insample<-confusionMatrix(data=factor(y_hat_insample) , 
                                   reference=factor(train$Pobre) , 
                                   mode="sens_spec" )$table
      
      
      cm_outsample<-confusionMatrix(data=factor(y_hat_outsample) , 
                                    reference=factor(test$Pobre) , 
                                    mode="sens_spec")$table
      
      
      # Confusion Matrix insample
      cm_insample
      # Confusion Matrix outsample
      cm_outsample
      
      #metricas
      
      acc_in <- Accuracy(y_true = train$Pobre, y_pred = y_hat_insample)
      acc_in <- round(100*acc_in, 2)
      pre_in <- Precision(y_true = train$Pobre, y_pred = y_hat_insample)
      pre_in <- round(100*pre_in, 2)
      recall_in <- Recall(y_true = train$Pobre, y_pred = y_hat_insample)
      recall_in <- round(100*recall_in, 2)
      f1_in <- F1_Score(y_true = train$Pobre, y_pred = y_hat_insample)
      f1_in <- round(100*f1_in, 2)
      
      acc_out <- Accuracy(y_true = test$Pobre, y_pred = y_hat_outsample)
      acc_out <- round(100*acc_out, 2)
      pre_out <- Precision(y_true = test$Pobre, y_pred = y_hat_outsample)
      pre_out <- round(100*pre_out, 2)
      recall_out <- Recall(y_true = test$Pobre, y_pred = y_hat_outsample)
      recall_out <- round(100*recall_out, 2)
      f1_out <- F1_Score(y_true = test$Pobre, y_pred = y_hat_outsample)
      f1_out <- round(100*f1_out, 2)
      
      resultados3 <- data.frame(Modelo = "Modelo 3: Grid search", Base = c("Train", "Test"), 
                                Accuracy = c(acc_in, acc_out), 
                                Precision = c(pre_in, pre_out),
                                Recall = c(recall_in, recall_out),
                                F1 = c(f1_in, f1_out))
      
      resultados3
      
      
      #Modelo  Base Accuracy Precision Recall    F1
      #1 Modelo 3: Grid search Train    87.16     89.57  95.01 92.21
      #2 Modelo 3: Grid search  Test    85.27     88.43  93.83 91.05
      
      
      
      
#-----------------------------arbol3------------------------------------------
      
      
      # Convertimos la marca a factor
      train$Pobre <- factor(train$Pobre)
      test$Pobre <- factor(test$Pobre)
      
      # Creamos modelo
      modelo <- decision_tree(
        cost_complexity = tune(),
        tree_depth = tune(),
        min_n = tune()
      ) %>%
        set_engine("rpart") %>%
        set_mode("classification")
      #creamos grilla
      tree_grid <- crossing(
        cost_complexity = c(0.0001),
        min_n = c(2, 14, 27),
        tree_depth = c(4, 8, 16)
      )
      #definimos CV
      
      set.seed(666)
      folds <- vfold_cv(train, strata = Pobre, v = 5)
      set.seed(666)
      modelo_cv <- tune_grid(
        modelo,
        Pobre ~ hacinamiento
        +rs_jefe_hogar+subsidio+
          +arriendo_estimado,
        resamples = folds,
        grid = tree_grid,
        metrics = metric_set(f_meas),
        control = control_grid(event_level = 'second')
      )
      
      collect_metrics(modelo_cv)
      autoplot(modelo_cv) + 
        theme_light() +
        labs(y = "F1 Score")
      
      # Escogemos el mejor modelo
      modelo <- finalize_model(modelo, select_best(modelo_cv))
      
      # Entrenamos el mejor modelo
      modelo_fit <- fit(modelo, Pobre ~ rs_jefe_hogar+hacinamiento+subsidio+
                          +arriendo_estimado, train)
      
      # Gráfica del modelo
      fancyRpartPlot(modelo_fit$fit, main = "Árbol con fine tuning", 
                     sub = "")
      
      # Importancia de las variables
      importancia <- varImp(modelo_fit$fit)
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
      
      # Evaluamos
      
      y_hat_insample <- predict(modelo_fit, train)$.pred_class
      y_hat_outsample <- predict(modelo_fit, test)$.pred_class
      
      
      cm_insample<-confusionMatrix(data=factor(y_hat_insample) , 
                                   reference=factor(train$Pobre) , 
                                   mode="sens_spec" )$table
      
      
      cm_outsample<-confusionMatrix(data=factor(y_hat_outsample) , 
                                    reference=factor(test$Pobre) , 
                                    mode="sens_spec")$table
      
      
      # Confusion Matrix insample
      cm_insample
      # Confusion Matrix outsample
      cm_outsample
      
      #metricas
      
      acc_in <- Accuracy(y_true = train$Pobre, y_pred = y_hat_insample)
      acc_in <- round(100*acc_in, 2)
      pre_in <- Precision(y_true = train$Pobre, y_pred = y_hat_insample)
      pre_in <- round(100*pre_in, 2)
      recall_in <- Recall(y_true = train$Pobre, y_pred = y_hat_insample)
      recall_in <- round(100*recall_in, 2)
      f1_in <- F1_Score(y_true = train$Pobre, y_pred = y_hat_insample)
      f1_in <- round(100*f1_in, 2)
      
      acc_out <- Accuracy(y_true = test$Pobre, y_pred = y_hat_outsample)
      acc_out <- round(100*acc_out, 2)
      pre_out <- Precision(y_true = test$Pobre, y_pred = y_hat_outsample)
      pre_out <- round(100*pre_out, 2)
      recall_out <- Recall(y_true = test$Pobre, y_pred = y_hat_outsample)
      recall_out <- round(100*recall_out, 2)
      f1_out <- F1_Score(y_true = test$Pobre, y_pred = y_hat_outsample)
      f1_out <- round(100*f1_out, 2)
      
      resultados <- data.frame(Modelo = "Modelo : Grid search", Base = c("Train", "Test"), 
                               Accuracy = c(acc_in, acc_out), 
                               Precision = c(pre_in, pre_out),
                               Recall = c(recall_in, recall_out),
                               F1 = c(f1_in, f1_out))
      
      resultados
      
      
      #Modelo  Base Accuracy Precision Recall    F1
      #1 Modelo : Grid search Train    83.27     85.54  95.18 90.10
      # 2 Modelo : Grid search  Test    83.00     85.40  94.93 89.92
      
      
      

#----------------------------------------------- R a n d o m   F o r e s t  ---------------------------------------------
      
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
      
      
      # Comando automático (a mi no me gusta)
      plot(modeloRF)
     
      # Comando manual
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
      
      # El mejor modelo es aquel que tiene mtry = X y min.node.size = X
      y_hat_insample2 = predict(modeloRF, newdata = x_train2)
      y_hat_outsample2 = predict(modeloRF, newdata = predicciones_general_t)
      modeloRF$coefnames
      
      #metricas 
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
      
--------# Importancia de las variables
      
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
      
    
#----------------------------------------------- E s t i m a c i ó n   f i n a l -----------------------------------------
#----------------------------------- C o n s t r u c c i o n   d e   l a   b a s e    t e s t ---------------------------------------
      #Sexo jefe hogar
      sex_jefe_hog<- as.data.frame(ifelse((test_personas$P6020==1 & test_personas$P6050==1),1,0))
      test_personas<- cbind(test_personas, sex_jefe_hog)
      names(test_personas)[names(test_personas)=='ifelse((test_personas$P6020 == 1 & test_personas$P6050 == 1), 1, 0)']<-'sexo_jefe_hogar'
      sexo_jefe_hog<-test_personas %>% group_by(id) %>% summarize(sexo_jefe_hogar=sum(sexo_jefe_hogar,na.rm = TRUE)) 
      summary(sexo_jefe_hog)
      #Edad jefe hogar
      edad_jefe_hogar<- (ifelse((test_personas$P6050==1),test_personas$P6040,0))
      test_personas<- cbind(test_personas, edad_jefe_hogar)
      edad_jefe_hog<-test_personas %>% group_by(id) %>% summarize(edad_jefe_hogar=sum(edad_jefe_hogar,na.rm = TRUE)) 
      summary(edad_jefe_hog)
      
      #Hacinamiento
      hacinamiento=test_hogares$Nper/test_hogares$P5010
      test_hogares<-cbind(test_hogares, hacinamiento)
      
      #Régimen de salud jefe de hogar
      rs_jefe_hogar<- (ifelse((test_personas$P6050==1),test_personas$P6100,0))
      test_personas<- cbind(test_personas, rs_jefe_hogar)
      rs_jefe_hog<-test_personas %>% group_by(id) %>% summarize(rs_jefe_hogar=sum(rs_jefe_hogar,na.rm = TRUE)) 
      summary(rs_jefe_hog)
      
      #Nivel educativo jefe hogar
      edu_jefe_hogar<- (ifelse((test_personas$P6050==1),test_personas$P6210,0))
      test_personas<- cbind(test_personas, edu_jefe_hogar)
      educ_jefe_hog<-test_personas %>% group_by(id) %>% summarize(edu_jefe_hogar=sum(edu_jefe_hogar,na.rm = TRUE)) 
      summary(educ_jefe_hog)
      
      # Ocupación jefe hogar
      ocupacion_jefe_hogar<- (ifelse((test_personas$P6050==1),test_personas$P6430,0))
      test_personas<- cbind(test_personas, ocupacion_jefe_hogar)
      ocupacion_jefe_hog<-test_personas %>% group_by(id) %>% summarize(ocupacion_jefe_hogar=sum(ocupacion_jefe_hogar,na.rm = TRUE)) 
      summary(ocupacion_jefe_hog)
      
      #Subsidios
      test_personas$P6585s1 = ifelse((test_personas$P6585s1==2),0,test_personas$P6585s1)
      test_personas$P6585s2 = ifelse((test_personas$P6585s2==2),0,test_personas$P6585s2)
      test_personas$P6585s3 = ifelse((test_personas$P6585s3==2),0,test_personas$P6585s3)
      test_personas$P6585s4 = ifelse((test_personas$P6585s4==2),0,test_personas$P6585s4)
      test_personas$P6585s1 = ifelse((test_personas$P6585s1==9),0,test_personas$P6585s1)
      test_personas$P6585s2 = ifelse((test_personas$P6585s2==9),0,test_personas$P6585s2)
      test_personas$P6585s3 = ifelse((test_personas$P6585s3==9),0,test_personas$P6585s3)
      test_personas$P6585s4 = ifelse((test_personas$P6585s4==9),0,test_personas$P6585s4)
      subsidio<-(test_personas$P6585s1+test_personas$P6585s2+test_personas$P6585s3+test_personas$P6585s4)
      test_personas<-cbind(test_personas,subsidio)
      sub_hog<-test_personas %>% group_by(id) %>% summarize(subsidio=sum(subsidio,na.rm = TRUE)) 
      
      #Suma de horas trabajadas por hogar 
      horas_tra_hogar<-test_personas %>% group_by(id) %>% summarize(P6800=mean(P6800,na.rm = TRUE)) 
      
      #Suma de horas trabajadas segundo empleo por hogar 
      horas_tra_hogar_2<-test_personas %>% group_by(id) %>% summarize(P7045=mean(P7045,na.rm = TRUE)) 
      
      
      #-------Merge 
      test_hogares<-left_join(test_hogares, sexo_jefe_hog)
      test_hogares<-left_join(test_hogares, edad_jefe_hog)
      test_hogares<-left_join(test_hogares, rs_jefe_hog)
      test_hogares<-left_join(test_hogares, educ_jefe_hog)
      test_hogares<-left_join(test_hogares, ocupacion_jefe_hog)
      test_hogares<-left_join(test_hogares, sub_hog)
      test_hogares<-left_join(test_hogares, horas_tra_hogar)
      test_hogares<-left_join(test_hogares, horas_tra_hogar_2)
      colnames(test_hogares)
      
      
      #-------Renonbrar variables y cambiar missings
      names(test_hogares)[names(test_hogares)=='P5000']<-'Ncuartos'
      names(test_hogares)[names(test_hogares)=='P5010']<-'Ncuartos_dormir'
      names(test_hogares)[names(test_hogares)=='P5090']<-'Tipo_vivienda'
      names(test_hogares)[names(test_hogares)=='P6800']<-'Horas_trabajo1'
      names(test_hogares)[names(test_hogares)=='P7045']<-'Horas_trabajo2'
      test_hogares$P5130 = ifelse(is.na(test_hogares$P5130)==T,0,test_hogares$P5130)
      test_hogares$P5140 = ifelse(is.na(test_hogares$P5140)==T,0,test_hogares$P5140)
      test_hogares$Horas_trabajo1 = ifelse(is.na(test_hogares$Horas_trabajo1)==T,0,test_hogares$Horas_trabajo1)
      test_hogares$Horas_trabajo2 = ifelse(is.na(test_hogares$Horas_trabajo2)==T,0,test_hogares$Horas_trabajo2)
      
      test_hogares$subsidio<-(ifelse((test_hogares$subsidio>0),1,0))
      arriendo_estimado<-test_hogares$P5130+test_hogares$P5140
      test_hogares<-cbind(test_hogares,arriendo_estimado$subsidio<-(ifelse((test_hogares$subsidio>0),1,0))
                          test_hogares<-cbind(test_hogares,arriendo_estimado)
                          
                          
                          test_hogares2<-test_hogares
                          
                          
                          # 1.Variables como factor 
                          test_hogares2$Dominio<-as.factor(test_hogares2$Dominio)
                          test_hogares2$Tipo_vivienda<-as.factor(test_hogares2$Tipo_vivienda)
                          test_hogares2$rs_jefe_hogar<-as.factor(test_hogares2$rs_jefe_hogar)
                          test_hogares2$edu_jefe_hogar<-as.factor(test_hogares2$edu_jefe_hogar)
                          test_hogares2$ocupacion_jefe_hogar<-as.factor(test_hogares2$ocupacion_jefe_hogar)
                          
                          #Logaritmo del ingreso percapita
                          ing_per = test_hogares2$Ingpcug
                          ing_per<-ifelse((ing_per)==0,1,ing_per)
                          log_ing_per<- log(ing_per)
                          train_hogares2<-cbind(train_hogares2,log_ing_per)
                          
#--------------------------------------------------- F o r e s t  f i n a l ---------------------------------------------
                          install.packages("ranger")
                          library(ranger)
                          
                          y_train2<-upSampledTrain$Pobre
                          x_train2<-upSampledTrain
                          x_train2<-x_train2[,-62]
                          x_train2<-x_train2[,-61]
                          x_train2<-x_train2[,-60]
                          x_continuas=(upSampledTrain[,c(3,4,6,7,14,18,22,24,26)])
                          x_categoricas=(upSampledTrain[,c(2,5,17,19,20,21,25)])
                          # Ahora procedemos a dummyficar la base
                          x_categoricas<- model.matrix(~ ., x_categoricas) %>%
                            as.data.frame 
                          predicciones_general<- cbind(x_categoricas,x_continuas)
                          
                          #base para reemplazo del modelo
                          x_continuas_t=(test_hogares[c(3,4,6,7,8,9,13,14,15)]
                                         x_continuas_t<-scale(x_continuas_t,center=T,scale=T)
                                         x_categoricas_t=(test_hogares[,c(2,5,10,11,12,16,17)])
                                         
                                         
                                         # Ahora procedemos a dummyficar la base
                                         x_categoricas_t<- model.matrix(~ ., x_categoricas_t) %>%
                                           as.data.frame 
                                         
                                         x_categoricas_t<-x_categoricas_t[,-1]
                                         x_categoricas_t<-cbind(DominioBOGOTA,x_categoricas_t)
                                         predicciones_general_t<- cbind(x_categoricas_t,x_continuas_t)
                                         
                                         
                                         
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
                                         
                                         
                                         # El mejor modelo es aquel que tiene mtry = X y min.node.size = X
                                         y_hat_outsample2 = predict(modeloRF, newdata = predicciones_general_t)
                                         
                                         
                                         
                                         
                                         
                                         
#-------------------------------------------------- E l a s t i c  N e t ---------------------------------------------
                                         
                                         
                                         # Model Building : Elastic Net Regression
                                         custom <- trainControl(method = "repeatedcv",
                                                                number = 10,
                                                                repeats = 5,
                                                                verboseIter = TRUE)
                                         y_train=upSampledTrain$log_ing_per
                                         
                                         #modelo
                                         set.seed(12345)
                                         en <- train(y_train~.,
                                                     data=cbind(y_train,predicciones_general),
                                                     method='glmnet',
                                                     tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                                                           lambda = seq(0.0001,0.2,length=5)),
                                                     trControl=custom)
                                         
                                         #-------resultados 
                                         #test
                                         test_hogares<-cbind(test_hogares,test_hogares2$sexo_jefe_hogar)
                                         names(test_hogares)[names(test_hogares)=='`test_hogares2$sexo_jefe_hogar']<-"sexo_jefe_hogar"
                                         
                                         test_hogares<-cbind(test_hogares,test_hogares2$rs_jefe_hogar)
                                         names(test_hogares)[names(test_hogares)=='test_hogares2$rs_jefe_hogar']<-"rs_jefe_hogar"
                                         
                                         test_hogares<-test_hogares[,-2]
                                         test_hogares<-test_hogares[,-6]
                                         test_hogares<-test_hogares[,-6]
                                         test_hogares<-test_hogares[,-6]
                                         test_hogares<-test_hogares[,-8]
                                         test_hogares<-test_hogares[,-10]
                                         test_hogares<-test_hogares[,-12]
                                         test_hogares<-test_hogares[,-8] 
                                         test_hogares<-test_hogares[,-13]
                                         test_hogares<-test_hogares[,-14]
                                         
                                         test_hogares$Dominio<-as.factor(test_hogares$Dominio)
                                         test_hogares$Tipo_vivienda<-as.factor(test_hogares$Tipo_vivienda)
                                         test_hogares$rs_jefe_hogar<-as.factor(test_hogares$rs_jefe_hogar)
                                         test_hogares$edu_jefe_hogar<-as.factor(test_hogares$edu_jefe_hogar)
                                         test_hogares$ocupacion_jefe_hogar<-as.factor(test_hogares$ocupacion_jefe_hogar)
                                         
                                         
                                         #----------Modelo ganador 
                                         modelo<- glmnet(
                                           x           = predicciones_general,
                                           y           = y_train,
                                           alpha       = 0.444,
                                           lambda      = 0.0001,
                                           standardize = TRUE
                                         )
                                         
                                         predicciones_general_t<-as.matrix(predicciones_general_t)
                                         predicciones_test <- predict(modelo, newx = predicciones_general_t)
                                         y_hat_en_outsample<-exp(predicciones_test)
                                         y_hat_en_outsample <- as.numeric(ifelse(y_hat_en_outsample<test_hogares2$Lp,1,0))
                                         table(y_hat_en_outsample)
                                         
                                         
