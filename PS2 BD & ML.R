#-------PROBLEM SET 2 
#Regresión
#  Lasso 
#  Ridge 
#  regresión normal
#  subset selection (backward)
#  medida para mejorar la métrica

#Clasificación
#  K vecinos cercanos 
#  Arbol
#  RF
#  Logit 
#  Medida para mejorar la métrica

#-------Lectura bases 
train_hogares<-readRDS(here("../data/train_hogares.Rds"))
train_personas<-readRDS(here("../data/train_personas.Rds"))

#-------Unión de variables 
    install.packages("glmnet")
    require("here")
    require("tidyverse")
    require("glmnet")
    install.packages("modeest") 
    library(modeest)
    library(dplyr)

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
    
 #Estrato 
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
    
#Subsidios (cambiar # por 1 después del merge)
  
    subsidio<-(train_personas$P6585s1a1+train_personas$P6585s2a1+train_personas$P6585s3a1+train_personas$P6585s4a1)
    train_personas<-cbind(train_personas,subsidio)
    sub_hog<-train_personas %>% group_by(id) %>% summarize(subsidio=sum(subsidio,na.rm = TRUE)) 
    sub_hog<-(ifelse((sub_hog[,2]>0),1,0))
    table(sub_hog)

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
    train_hogares<-left_join(train_hogares, sum_des)
    train_hogares<-left_join(train_hogares, sum_des)
    train_hogares<-left_join(train_hogares, sum_des)
    train_hogares<-left_join(train_hogares, sum_des)
    train_hogares<-left_join(train_hogares, sum_des)
    train_hogares<-left_join(train_hogares, sum_des)
    train_hogares<-left_join(train_hogares, sum_des)
    train_hogares<-left_join(train_hogares, sum_des)
    train_hogares<-left_join(train_hogares, sum_des)
    train_hogares<-left_join(train_hogares, sum_des)
    colnames(train_hogares)
    
#-------Eliminar variables 
    train_hogares<-train_hogares[,-2]
    train_hogares<-train_hogares[,-6]
    
#-------Renonbrar variables 
    
    
#--------LASSO

X<-model.matrix(~.,train_personas)
y<-train_personas$Ingtot
lasso.mod <- glmnet(X, y, alpha = 1, lambda = 0)
lasso.mod$beta
