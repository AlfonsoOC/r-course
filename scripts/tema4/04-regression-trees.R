#Arboles de regression

#install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
library(caret)

bh <- read.csv("GitHub/r-course/data/tema4/BostonHousing.csv")

#La variable que voy a predecir es MEDV "variable de habitabilidad"
#datos de entrenamiento tomo el 70 % de los datos
t.id <- createDataPartition(bh$MEDV, p = .7, list = F)

# aqui realizo el arbol de regresion con "rpart"
#intentando predecir MEDV en funcion de todas las demascolumnas
bfit <- rpart(MEDV ~., data = bh[t.id,])
bfit

par(mfrow=c(1,1))

# prp nos permite reprsentar el arbol de manera rafica
prp(bfit, type = 2, nn=T,
    fallen.leaves = T, faclen = 4,
    varlen = 8, shadow.col = "gray")
#en la grafica podemos ver que la computadora utiliza para presecir solo algunas variables, aqui usa "LSTAT" varias veces RM y DIS

#La cptable nos muestra datos comprensibles de los nodos de los arboles de regresion
#CP se refiere al factor de complejidad del arbol
#nspli = numero de divisiones
#error de clasificacion total con raiz cuadrada apartir del nodo raiz
#error de validacion cruzada promedio
#desviacion estandar de la validacion cruzada del mejor arbol
bfit$cptable

# plotcp nos muestra que tan buenos son los arboles de distintos tamanios contra el valor del error relativo
#con esto se puede decidir cual es el mejor CP para estos datos
#con estos datos nosotros en este caso cpodriamos cortar el arbor a CP= 5 
plotcp(bfit)

#aqui utilizamos el CP del 6 para podar el arbol 
bfitpruned <- prune(bfit, cp = 0.01499132 )

#volvemos a hacer el arbol con este CP
prp(bfitpruned, type = 2, nn=T,
    fallen.leaves = T, faclen = 4,
    varlen = 8, shadow.col = "gray")

#aqui aislamos los datos del conjunto de prediccion y los comparamos con las predicciones echas por nuestro arbol original
preds <- predict(bfit, bh[t.id,])
sqrt(mean((preds - bh[t.id,]$MEDV)^2)) # este es el error 

preds <- predict(bfit, bh[-t.id, ])
sqrt(mean((preds - bh[-t.id,]$MEDV)^2))

#aqui aislamos los datos del conjunto de prediccion y los comparamos con las predicciones echas por nuestro arbol recortado
preds <- predict(bfitpruned, bh[t.id,])
sqrt(mean((preds - bh[t.id,]$MEDV)^2))

preds <- predict(bfitpruned, bh[-t.id, ])
sqrt(mean((preds - bh[-t.id,]$MEDV)^2))

#es buena idea cortar los arboles ya que te da arboles mas sencillos ademas de que puedes prevenir overfiting




#predictores categóricos

ed <- read.csv("GitHub/r-course/data/tema4/education.csv")
ed$region <- factor(ed$region) #Region es un factor 
t.id <- createDataPartition(ed$expense, p = 0.7, list = F)
fit <- rpart(expense ~ region+urban+income+under18, data = ed[t.id,]) #predecir expense en funcion de region, urgan, income y under18
prp(fit, type = 2, nn=T, fallen.leaves = T, 
    faclen=4, varlen=8, shadow.col = "gray")

#METODOS DE ENSAMBLAJE

#Bagging 
#combina de manera conjunta las predicciones de diferentes arboles para dar un mejor resultado
#es bueno para metodos con alta varianza 
#install.packages("ipred")
library(ipred)
bagging.fit <- bagging(MEDV~., data=bh[t.id, ]) #usando los mismos treding id, prediciendo MEDV en funcion de todo lo demas
prediction.t <- predict(bagging.fit, bh[t.id,]) #prediccion sobre el conjunto de entrenamiento
sqrt(mean((prediction.t-bh[t.id,]$MEDV)^2)) #calcular el error cuadratico medio

prediction.v <- predict(bagging.fit, bh[-t.id,]) # prediccion en conjunto de validacion 
sqrt(mean((prediction.v-bh[-t.id,]$MEDV)^2))


#Boosting
#gradient boosting machines (gbm) que es el paquete que usaremos
#install.packages("gbm")
library(gbm)

gbmfit <- gbm(MEDV~., data = bh[t.id,], distribution = "gaussian") # predecir MEDV en funcion de las demas variables usando la distribucion gausiana
prediction.t <- predict(gbmfit, bh[t.id,])
sqrt(mean((prediction.t-bh[t.id,]$MEDV)^2))
