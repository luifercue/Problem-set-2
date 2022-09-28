#-------PROBLEM SET 2 
#Regresión
#  Lasso 
#  Ridge 
#  Elastic Net
#  regresión normal
#  subset selection (backward)
#  medida para mejorar la métrica

#Clasificación
#  K vecinos cercanos 
#  Arbol
#  RF
#  Lasso logit
#  LDA
#  Medida para mejorar la métrica

#-------Lectura bases 
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
    
    colnames(train_hogares)
    for i {
      "id"          
      "Clase"       
      "Dominio"      
      "Lp"          
      "Pobre"     
      "Npobres"     
      "Depto"       
      "Hacinamiento= Nper/P5010"
      "P5090= Vivienda propia totalmente pagado, propia la están pagando, 
      en arriendo o subarriendo, usufructo, posesión sin título"
      "PAUG=Npersug/Nper= Número de personas en la unidad de gasto"
      "Ingtotugarr=Ingreso total de la unidad de gasto con la imputación del arriendo" 
      "Ingpcug=Ingreso per capita de Ingtotugarr" }
    
    colnames(train_personas)
    for i ¨{
      "id"        
      "Clase"     
      "estrato=Estrato1 (media)" 
      "Sexo_jefe_hog=P6020 y P6050"    
      "edad_jefe_hogar=P6020 P6050" 
      "rs_jefe_hog (P6100 P6050)"     
      "educ_jefe_hog (P6210)"     
      "P6430 
          a: obrero empleado empresa particular
          b. obrero empleado del gob
          c. empleado doméstico
          d. Trabajador cuenta propia
          e. Patrón empleador
          f. Trabajador familiar sin remuneración
          g. Trabajador sin remuneración externo
          h. jornalero peón 
          i. otro
          
          Clasificar jefe de hogar       "     
      
      #subsidios (El hogar recibe algún subsidio)
      "P6585s1 alimentación" "P6585s2 transporte"  "P6585s3 familiar"
      "P6585s4 educativo" "1=si, 9=NS"
      
      "P6800 (suma de horas 1er trabajo por hogar)"    
      "P6920 (cotiza 1=si, 2=no, 3=pensionado) jefe hogar " 
      "P7045 (suma horas 2do trabajo por hogar) "   
      "PET+OC trabajo infantil dummy"
      "TD=Des ds/pet x hogar"
      
      "estrato, sexo_jefe_hog,edad_jefe_hog, hacinamiento, 
      paug,rs_jefe_hog, educ_jefe_hog, ocupacion_jefe_hog, 
      sub_hog, horas_tra_hogar, cot_jefe_hog,horas_tra_hogar_2,
      sum_pet, sum_des"
      
      26 variables
    }
    
#----------------------------------- C o n s t r u c c i o n   d e   l a   b a s e ---------------------------------
        #Estrato (no está en test)
            estrato<-train_personas %>% group_by(id) %>% summarize(Estrato1=mean(Estrato1,na.rm = TRUE)) 
            table(estrato, train_personas$Estrato1)
            
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
            
        #Personas que aportan a la unidad de gasto (PAUG)
            paug=(train_hogares$Npersug/train_hogares$Nper)
            train_hogares<-cbind(train_hogares, paug)
            table(paug)
            
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
            train_hogares<-train_hogares[,-30]
            train_personas<-train_personas[,-141]
            
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
        
        #Cotiza pensión jefe de hogar
            cot_jefe_hogar<- (ifelse((train_personas$P6050==1),train_personas$P6920,0))
            train_personas<- cbind(train_personas, cot_jefe_hogar)
            cot_jefe_hog<-train_personas %>% group_by(id) %>% summarize(cot_jefe_hogar=sum(cot_jefe_hogar,na.rm = TRUE)) 
           
        #Suma de horas trabajadas segundo empleo por hogar 
            horas_tra_hogar_2<-train_personas %>% group_by(id) %>% summarize(P7045=mean(P7045,na.rm = TRUE)) 
            
        #TD por hogar 
            #reemplazar missing de des por 0
            train_personas$Des = ifelse(is.na(train_personas$Des)==T,0,train_personas$Des)
            #Suma Pet
            sum_pet<-train_personas %>% group_by(id) %>% summarize(Pet=sum(Pet,na.rm = TRUE)) 
            #Suma Des
            sum_des<-train_personas %>% group_by(id) %>% summarize(Des=sum(Des,na.rm = TRUE)) 
            
            
        #-------Merge 
            train_hogares<-left_join(train_hogares, sum_des)
            train_hogares<-left_join(train_hogares, sexo_jefe_hog)
            train_hogares<-left_join(train_hogares, edad_jefe_hog)
            train_hogares<-left_join(train_hogares, hacinamiento)
            train_hogares<-left_join(train_hogares, paug)
            train_hogares<-left_join(train_hogares, rs_jefe_hog)
            train_hogares<-left_join(train_hogares, educ_jefe_hog)
            train_hogares<-left_join(train_hogares, ocupacion_jefe_hog)
            train_hogares<-left_join(train_hogares, sub_hog)
            train_hogares<-left_join(train_hogares, horas_tra_hogar)
            train_hogares<-left_join(train_hogares, cot_jefe_hog)
            train_hogares<-left_join(train_hogares, horas_tra_hogar_2)
            train_hogares<-left_join(train_hogares, sum_pet)
            train_hogares<-left_join(train_hogares, sum_des)
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
            train_hogares<-train_hogares[,-2]
            train_hogares<-train_hogares[,-6]
            arriendo_estimado<-train_hogares$P5130+train_hogares$P5140
            train_hogares<-cbind(train_hogares,arriendo_estimado)
            train_hogares<-train_hogares[,-6]
            train_hogares<-train_hogares[,-6]
            train_hogares<-train_hogares[,-8]
            train_hogares<-train_hogares[,-10]
            train_hogares<-train_hogares[,-12]
            train_hogares<-train_hogares[,-13] 
            train_hogares<-train_hogares[,-13]
            train_hogares<-train_hogares[,-14]
            
            TD<-train_hogares$Des/train_hogares$Pet
            train_hogares<-cbind(train_hogares,TD)
            train_hogares<-train_hogares[,-26]
            train_hogares<-train_hogares[,-25]
            train_hogares$subsidio<-(ifelse((train_hogares$subsidio>0),1,0))
            #back up
            train_hogares2<-train_hogares
            #Logaritmo del ingreso percapita
            ing_per = train_hogares2$Ingpcug
            ing_per<-ifelse((ing_per)==0,1,ing_per)
            log_ing_per<- log(ing_per)
            train_hogares2<-cbind(train_hogares2,log_ing_per)
            
            
            # Variables como factor 
            train_hogares2$Dominio<-as.factor(train_hogares2$Dominio)
            train_hogares2$Tipo_vivienda<-as.factor(train_hogares2$Tipo_vivienda)
            train_hogares2$rs_jefe_hogar<-as.factor(train_hogares2$rs_jefe_hogar)
            train_hogares2$edu_jefe_hogar<-as.factor(train_hogares2$edu_jefe_hogar)
            train_hogares2$ocupacion_jefe_hogar<-as.factor(train_hogares2$ocupacion_jefe_hogar)

#----------------------------------- E s t a d í s t i c a s   D e s c r i p t i v a s ---------------------------------
          
              table (train_hogares$Pobre) #131936  33024
            
#----------------------------------- D i v i s i ó n  d e  M u e s t r a ---------------------------------
            set.seed(12345) 
            train_hogares2 <- train_hogares2 %>%
              mutate(holdout= as.logical(1:nrow(train_hogares2) %in%
                                           sample(nrow(train_hogares2), nrow(train_hogares2)*.2))
              )
            test<-train_hogares2[train_hogares2$holdout==T,] #32.992
            train<-train_hogares2[train_hogares2$holdout==F,] #131.968
            
#----------------------------------- M o d e l o s   d e  e s t i m a c i ó n ---------------------------------
#-------------------------------------------------------------------------------------------------------------
#                                              MODELO DE REGRESIÓN 
#-------------------------------------------------------------------------------------------------------------
#-----------#Modelo 1
            modelo1<-lm(log_ing_per~ Tipo_vivienda+ rs_jefe_hogar+edu_jefe_hogar+ Ncuartos + Ncuartos_dormir+ 
                          Nper+Npersug+ hacinamiento+edad_jefe_hogar+Horas_trabajo1+Horas_trabajo2+
                          arriendo_estimado+ocupacion_jefe_hogar+Dominio+subsidio, data=train)
            summary(modelo1)
            
            modelo2<-lm(log_ing_per~ Tipo_vivienda+ rs_jefe_hogar+edu_jefe_hogar+ Ncuartos_dormir+ 
                          Npersug+ hacinamiento+edad_jefe_hogar+Horas_trabajo1+Horas_trabajo2+
                          arriendo_estimado+ocupacion_jefe_hogar+Dominio+subsidio, data=train)
            summary(modelo2)
            

 #-----------#MSE 
            test$modelo1<-predict(modelo1,newdata = test)
            with(test,mean((log_ing_per-modelo1)^2)) #1.084773
            
            test$modelo2<-predict(modelo2,newdata = test)
            with(test,mean((log_ing_per-modelo2)^2)) #1.100184 
            
            
              #Gráfico de coeficientes modelo 2  
                      
            df_coeficientes_reg2 <- modelo2$coefficients %>%
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
            
#-----------Evaluamos el modelo de regresión lineal
            y_hat_in1 <- predict(modelo2, newdata = train)
            y_hat_out1 <- predict(modelo2, newdata = test)
            
            # Métricas dentro y fuera de muestra. Paquete MLmetrics
            r2_in1 <- R2_Score(y_pred = exp(y_hat_in1), y_true = exp(train$log_ing_per))
            rmse_in1 <- RMSE(y_pred = exp(y_hat_in1), y_true = exp(train$log_ing_per))
            
            r2_out1 <- R2_Score(y_pred = exp(y_hat_out1), y_true = exp(test$log_ing_per))
            rmse_out1 <- RMSE(y_pred = exp(y_hat_out1), y_true = exp(test$log_ing_per))
            
            resultados <- data.frame(Modelo = "Regresión lineal", 
                                     Muestra = "Dentro",
                                     R2_Score = r2_in1, RMSE = rmse_in1) %>%
              rbind(data.frame(Modelo = "Regresión lineal", 
                               Muestra = "Fuera",
                               R2_Score = r2_out1, RMSE = rmse_out1))
            
#-------------------------------------------------------------------------------------------------------------
#                                                   LASSO
#-------------------------------------------------------------------------------------------------------------
#-----------Estandarizar variables continuas train
            train[,c(3,4,6,7,8,14,18,22,24,26)]<- scale(train[,c(3,4,6,7,8,14,18,22,24,26)],center=T,scale=T)
            #Estandarizar variables continuas test
            test[,c(3,4,6,7,8,14,18,22,24,26)]<- scale(test[,c(3,4,6,7,8,14,18,22,24,26)],center=T,scale=T)
            x=train[,c(2,3,4,5,6,7,14,17,19,20,21,22,24,25,26)]
            x2
            # Ahora procedemos a dummyficar la base
            x <- model.matrix(~ Dominio+ Tipo_vivienda+ rs_jefe_hogar+edu_jefe_hogar
                              +ocupacion_jefe_hogar+subsidio, x) %>%
              as.data.frame()
            
                   
            modelo_lasso <- glmnet(
              x = x,
              y = train$log_ing_per,
              alpha = 1,
              nlambda = 5000,
              standardize = FALSE
            )
          
            #Lasso para lambda distinto (establecer grilla)
            
            # Analicemos cómo cambian los coeficientes para diferentes lambdas
            regularizacion <- modelo_lasso$beta %>%    
              as.matrix() %>%
              t() %>% 
              as_tibble() %>%
              mutate(lambda = modelo_lasso$lambda)
            
            regularizacion <- regularizacion %>%  
              pivot_longer(
                cols = !lambda, 
                names_to = "predictor",
                values_to = "coeficientes"
              )
            
#--------Gráfico de Lasso
            regularizacion %>%
            ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
            geom_line() +
            scale_x_log10(
              breaks = scales::trans_breaks("log10", function(x) 10^x),
              labels = scales::trans_format("log10",
                                            scales::math_format(10^.x))
            ) +
            labs(title = "Coeficientes del modelo en función de la regularización (Lasso)", x = "Lambda", y = "Coeficientes") +
            theme_bw() +
            theme(legend.position="bottom")
          
            # ¿Cómo escoger el mejor lambda? 
            # Veamos cuál es el mejor prediciendo (fuera de muestra)
            # En este caso vamos a crear la predicción para cada uno de los
            # 5000 lambdas seleccionados
            predicciones_lasso <- predict(modelo_lasso, 
                                        newx = as.matrix(x))
            lambdas_lasso <- modelo_lasso$lambda
          
            # Cada predicción se va a evaluar
            resultados_lasso <- data.frame()
            for (i in 1:length(lambdas_lasso)) {
              l <- lambdas_lasso[i]
              y_hat_out2 <- predicciones_lasso[, i]
              r22 <- R2_Score(y_pred = y_hat_out2, y_true = test$log_ing_per)
              rmse2 <- RMSE(y_pred = y_hat_out2, y_true = test$log_ing_per)
              resultado <- data.frame(Modelo = "Lasso",
                                      Muestra = "Fuera",
                                      Lambda = l,
                                      R2_Score = r22, 
                                      RMSE = rmse2)
              resultados_lasso <- bind_rows(resultados_lasso, resultado)
              }
          
            ggplot(resultados_lasso, aes(x = Lambda, y = RMSE)) +
            geom_point() +
            geom_line() +
            theme_bw() +
            scale_y_continuous(labels = scales::comma)
          
            ggplot(resultados_lasso, aes(x = Lambda, y = R2_Score)) +
            geom_point() +
            geom_line() +
            theme_bw() +
            scale_y_continuous(labels = scales::comma)
          
          
            filtro <- resultados_lasso$RMSE == min(resultados_lasso$RMSE)
            mejor_lambda_lasso <- resultados_lasso[filtro, "Lambda"]
            
            # Guardamos el mejor Lasso
            y_hat_in2 <- predict.glmnet(modelo_lasso,
                                      newx = as.matrix(x),
                                      s = mejor_lambda_lasso)
            y_hat_out2 <- predict.glmnet(modelo_lasso,
                                       newx = as.matrix(x),
                                       s = mejor_lambda_lasso)
          
            # Métricas dentro y fuera de muestra. Paquete MLmetrics
            r2_in2 <- R2_Score(y_pred = exp(y_hat_in2), y_true = exp(train$log_ing_per))
            rmse_in2 <- RMSE(y_pred = exp(y_hat_in2), y_true = exp(train$log_ing_per))
            
            r2_out2 <- R2_Score(y_pred = exp(y_hat_out2), y_true = exp(y_test))
            rmse_out2 <- RMSE(y_pred = exp(y_hat_out2), y_true = exp(y_test))
            
            # Guardamos el desempeño
            resultados2 <- data.frame(Modelo = "Lasso", 
                                    Muestra = "Dentro",
                                    R2_Score = r2_in2, RMSE = rmse_in2) %>%
            rbind(data.frame(Modelo = "Lasso", 
                             Muestra = "Fuera",
                             R2_Score = r2_out2, RMSE = rmse_out2))
          
            # Juntamos resultados con regresión lineal
            resultados <- rbind(resultados, resultados2)


#-------------------------------------------------------------------------------------------------------------------------------------
#                                   M o d e l o s  d e  C l a s i f i c a c i ó n
#------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------- B a l a n c e o   d e   l a   m u e s t r a ---------------------------------
#----------------- Gráfico de desbalanceo
            ggplot(train, aes(x = Pobre)) +
            geom_bar(fill = "darkblue") +
            theme_bw() +
            labs(title = " ¿El hogar es pobre? 
                        si=1, no=0",
                 x = "",
                 y = "Frecuencia") +
            coord_flip()
  #----------------- Balancear la muestra
            prop.table(table(train$Pobre))  #0.8001031 0.1998969 

    #Dummyficamos ANTES de partir la base en train/test
          train_s <- data.frame(train_s)
          test_s <- data.frame(test_s)
          train <- data.frame(train)
          test <- data.frame(test)
          
          train_s$infielTRUE <- as.numeric(train_s$infielTRUE)
          modelo1 <- lm(formula = infielTRUE ~ ., data = train_s)
          probs_insample1 <- predict(modelo1, train_s)
          probs_insample1[probs_insample1 < 0] <- 0
          probs_insample1[probs_insample1 > 1] <- 1
          probs_outsample1 <- predict(modelo1, test_s)
          probs_outsample1[probs_outsample1 < 0] <- 0
          probs_outsample1[probs_outsample1 > 1] <- 1
  
      # Convertimos la probabilidad en una predicción
      y_hat_insample1 <- as.numeric(probs_insample1 > 0.5)
      y_hat_outsample1 <- as.numeric(probs_outsample1 > 0.5)
      
      acc_insample1 <- Accuracy(y_pred = y_hat_insample1, y_true = train$infielTRUE)
      acc_outsample1 <- Accuracy(y_pred = y_hat_outsample1, y_true = test$infielTRUE)
      
      pre_insample1 <- Precision(y_pred = y_hat_insample1, y_true = train$infielTRUE, positive = 1)
      pre_outsample1 <- Precision(y_pred = y_hat_outsample1, y_true = test$infielTRUE, positive = 1)
      
      rec_insample1 <- Recall(y_pred = y_hat_insample1, y_true = train$infielTRUE, positive = 1)
      rec_outsample1 <- Recall(y_pred = y_hat_outsample1, y_true = test$infielTRUE, positive = 1)
      
      f1_insample1 <- F1_Score(y_pred = y_hat_insample1, y_true = train$infielTRUE, positive = 1)
      f1_outsample1 <- F1_Score(y_pred = y_hat_outsample1, y_true = test$infielTRUE, positive = 1)
      
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
      metricas1 %>%
        kbl(digits = 2)  %>%
        kable_styling(full_width = T)
  
#---------------------- Oversamplig 
        # Implementamos oversampling
        train$PobreTRUE <- factor(train$PobreTRUE)
        train_s2 <- recipe(ProbreTRUE ~ ., data = x) %>%
          themis::step_smote(PobreTRUE, over_ratio = 1) %>%
          prep() %>%
          bake(new_data = NULL)
  
        prop.table(table(x$PobreTRUE))
  
  
#----------------------------------- L a s s o - L o g i t  -----------------------------------------
        
  

                                   
                                   
#---------------------------------------- T r e e s ---------------------------------
  
#--------------------------------- R a n d o m   F o r e s t  ---------------------------------
#--------------- Tunear la grilla
        gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                                n.trees = (1:30)*50, 
                                shrinkage = 0.1,
                                n.minobsinnode = 20)
        
        nrow(gbmGrid)
        
        set.seed(825)
        gbmFit2 <- train(Class ~ ., data = training, 
                         method = "gbm", 
                         trControl = fitControl, 
                         verbose = FALSE, 
                         ## Now specify the exact models 
                         ## to evaluate:
                         tuneGrid = gbmGrid)
        gbmFit2
        
        
        
#---------------------------------M E J O R A R   M O D E L O   G A N A D O R ---------------------------------
#------------------------------------------------ T U N E A R  ---------------------------------------
        
#------------Métricas 
        The function trainControl generates parameters that further control how models are created, with possible values:
          
#---ROC  
          set.seed(825)
        gbmFit3 <- train(Class ~ ., data = training, 
                         method = "gbm", 
                         trControl = fitControl, 
                         verbose = FALSE, 
                         tuneGrid = gbmGrid,
                         ## Specify which metric to optimize
                         metric = "ROC")
        gbmFit3
#-------------- Model tuning: Maximizar la capacidd predictiva del modelo (para logit) 
        
        ctrl def <- trainControl(method = "cv",
                                 number = 5,
                                 summaryFunction = defaultSummary,
                                 classProbs = TRUE,
                                 verbose=FALSE,
                                 savePredictions = T)
#______________ Accuracy y Kappa 
        set.seed(1410)
        mylogit caret def <- train(
          Default ~amount+installment+age+ historygood + historypoor + purposeusedcar+ purposegoods.repair + purposeedu + foreigngerman + rentTRdata = training,
          method = "glm", #for logit
          trControl = ctrl def,
          family = "binomial",
          preProcess = c("center", "scale")
        )
        mylogit caret def
        
        
        ctrl two <- trainControl(method = "cv",
                                 number = 5,
                                 summaryFunction = twoClassSummary,
                                 classProbs = TRUE,
                                 verbose=FALSE,
                                 savePredictions = T)
        
#______________ ROC, Sensibilidad y especificidad
        
        set.seed(1410)
        mylogit caret two <- train(
          Default ~amount+installment+age+ historygood + historypoor + purposeusedcar+ purposegoods.repair + purposeedu + foreigngerman + rentTRdata = training,
          method = "glm", #for logit
          trControl = ctrl two,
          family = "binomial",
          preProcess = c("center", "scale")
        )
        
        
        #------------ 5 Métricas juntas
        
        fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
        ctrl<- trainControl(method = "cv",
                            number = 5,
                            summaryFunction = fiveStats,
                            classProbs = TRUE,
                            verbose=FALSE,
                            savePredictions = T)
        #logit
        set.seed(1410)
        mylogit caret <- train(
          Default ~amount+installment+age+ historygood + historypoor + purposeusedcar+ purposegoods.repair + purposeedu + foreigngerman + rentTRdata = training,
          method = "glm", #for logit
          trControl = ctrl,
          family = "binomial",
          preProcess = c("center", "scale")
        )
        
#-------------- Model tuning: Maximizar la capacidd predictiva del modelo (para losso) 
        #Model(glmnet) method (glmnet) Type (Classification), Regression	libraries(glmnet, Matrix)	Tunning parameters (alpha, lambda)
        #-----Lasso
        lambda grid <- 10^seq(-4, 0.01, length = 10) #en la practica se suele usar una grilla de 200 o 300
        lambda grid
        
#------------ 5 Métricas juntas
        
        set.seed(1410)
        mylogit lasso acc <- train(
          Default ~amount+installment+age+ historygood + historypoor + purposeusedcar+ purposegoods.repair + purposeedu + foreigngerman + rentTRdata = training,
          method = "glmnet",
          trControl = ctrl,
          family = "binomial",
          metric = "Accuracy",
          tuneGrid = expand.grid(alpha = 0,lambda=lambda grid),
          preProcess = c("center", "scale")
        )
        
        mylogit lasso acc
        
#------ROC        
        set.seed(1410)
        mylogit lasso roc <- train(
          Default ~amount+installment+age+ historygood + historypoor + purposeusedcar+ purposegoods.repair + purposeedu + foreigngerman + rentTRdata = training,
          method = "glmnet",
          trControl = ctrl,
          family = "binomial",
          metric = "ROC",
          tuneGrid = expand.grid(alpha = 0,lambda=lambda grid),
          preProcess = c("center", "scale")
        )
#------ Sensibilidad      
        
        set.seed(1410)
        mylogit caret sens <- train(
          Default ~amount+installment+age+ historygood + historypoor + purposeusedcar+ purposegoods.repair + purposeedu + foreigngerman + rentTRdata = training,
          method = "glmnet",
          trControl = ctrl,
          family = "binomial",
          metric = "Sens",
          tuneGrid = expand.grid(alpha = 0,lambda=lambda grid),
          preProcess = c("center", "scale")
        )
        
#------ Cutoff óptimo     
        
        evalResults <- data.frame(Default = evaluation$Default)
        evalResults$Roc <- predict(mylogit lasso roc,
                                   newdata = evaluation,
                                   type = "prob")[,1]