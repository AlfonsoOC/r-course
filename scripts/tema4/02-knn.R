#install.packages("FNN")

#KNN neibergs requiere que todos los predictores sean variables numericas por que queremos 
#obtener un valor numerico, asi que primero debemos convertir todos las variables categoricas 
#en este caso las regiones a variables dummies y hace un rescalado de variables numericas en el 
#intervalo de 0 a 1 


library(dummies)
library(FNN)
library(scales)
library(caret) #nos permite hacer las particiones

edu <- read.csv("../../data/tema4/education.csv")
#se generan variables dummies y se agregan a la tabla como una nuevas columnas
#dumies convierte variables a 0 o 1
dms <- dummy(edu$region, sep = "_")
edu <- cbind(edu, dms)

#para reescalar la seccion urban, income y under18 (se reescalo entre 0 y 1)
edu$urban.s <- rescale(edu$urban)
edu$income.s <- rescale(edu$income)
edu$under18.s <- rescale(edu$under18)

#se reparten los datos para hacer el machine learning con un grupo de temporales y uno de entrenamiento
set.seed(2018)
t.id <- createDataPartition(edu$expense, p=0.6, list = F)
tr <- edu[t.id, ]
temp <- edu[-t.id, ]
# despues reparto los datos temporales entre testing y validacion (aqui los parto a la mitad p=0.5)
v.id <- createDataPartition(temp$expense, p=0.5, list = F)
#me quedo con una seccion validacion (val)
val <- temp[v.id,]
#y para test (test)
test <- temp[-v.id,]

#esta es la regresion numero 1 (o modelo #1)
reg1 <- knn.reg(tr[,7:12], val[,7:12], tr$expense, k=1,
                algorithm = "brute")
rmse1 <- sqrt(mean((reg1$pred-val$expense)^2))
rmse1

reg2 <- knn.reg(tr[,7:12], val[,7:12], tr$expense, k=2,
                algorithm = "brute")
rmse2 <- rmse(val$expense, reg2$pred)
rmse2

reg3 <- knn.reg(tr[,7:12], val[,7:12], tr$expense, k=3,
                algorithm = "brute")
rmse3 <- rmse(val$expense, reg3$pred)
rmse3

df = data.frame(actual = val$expense, pred = reg3$pred)
plot(df)
abline(0,1)



reg4 <- knn.reg(tr[,7:12], val[,7:12], tr$expense, k=4,
                algorithm = "brute")
rmse4 <- rmse(val$expense, reg4$pred)
rmse4

reg5 <- knn.reg(tr[,7:12], val[,7:12], tr$expense, k=5,
                algorithm = "brute")
rmse5 <- rmse(val$expense, reg5$pred)
rmse5

errors = c(rmse1, rmse2, rmse3, rmse4, rmse5)

plot(errors, type = 'o', xlab = "k", ylab = "RMSE")


reg.test <- knn.reg(tr[,7:12], test[,7:12], tr$expense, k=3,
                algorithm = "brute")
rmse.test <- rmse(test$expense, reg.test$pred)
rmse.test

df = data.frame(actual = test$expense, pred = reg.test$pred)
plot(df)
abline(0,1)



#esta es otra opcion en la que solo realizamos dos particiones, una para validar y otra para entrenar

t.id <- createDataPartition(edu$expense, p = 0.7, list = F)
tr <- edu[t.id, ] #training
val <- edu[-t.id,] # validar
reg <- knn.reg(tr[,7:12], 
               test = NULL, #como puedes ver aqui no hay conjunto de testing
               y = tr$expense,
               k = 3, algorithm = "brute")
rmse.reg <- sqrt(mean(reg$residuals^2))
rmse.reg

##Función para automatizar KNN
rda.knn.reg <- function(tr_predictor, val_predictors,
                          tr_target, val_target, k){
  library(FNN)
  res <- knn.reg(tr_predictor, val_predictors,
                 tr_target, k, algorithm = "brute")
  rmserror <- sqrt(mean((val_target - res$pred)^2))
  cat(paste("RMSE para k = ", toString(k), ": ", rmserror,"\n", sep = ""))
  rmserror
}


##Función para realizar múltiples KNN
rda.knn.reg.multi <- function(tr_predictors, val_predictors,
                                tr_target, val_target, start_k, end_k){
  rms_errors <- vector()
  for(k in start_k:end_k){
    rms_error <- rda.knn.reg(tr_predictors, val_predictors,
                               tr_target, val_target, k)
    rms_errors <- c(rms_errors, rms_error)
  }
  plot(rms_errors, type = 'o', xlab = "k", ylab = "RMSE")
}


rda.knn.reg.multi(tr[,7:12], val[,7:12], 
                    tr$expense, val$expense, 1,10)
