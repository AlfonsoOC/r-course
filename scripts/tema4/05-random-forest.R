#Random forest para predecir el valor promedio del data set de las casas de boston

library(randomForest)
library(caret)
bh <- read.csv("GitHub/r-course/data/tema4/BostonHousing.csv")

set.seed(2018)
t.id <- createDataPartition(bh$MEDV, p = 0.7, list = F) #70% para entrenar

mod <- randomForest(x = bh[t.id, 1:13], y = bh[t.id, 14], #variables independientes x, variable dependiente es y (la que quiero estimar)
                    ntree = 1000, #numero de arboles que quiero que haga
                    xtest = bh[-t.id, 1:13], ytest = bh[-t.id, 14], #conjunto independiente de testing xtest, y ytest es el dependiente  
                    importance = T, keep.forest = T)  #importance =T si quieres computar las puntuaciones de todas las variables predictoras

mod #aqui se imprime por consola el modelo

mod$importance #la variable importance del modelo, como su nombre nos dice nos muestra que variables son mas importantes para predecir el modelo


plot(bh[t.id,]$MEDV, predict(mod, newdata = bh[t.id,]), 
     xlab = "Actual", ylab = "Predichos")
abline(0,1) #el resultado es bastante bueno y se puede ver mostrando una linea que pasa por 0 y de pendiente 1


plot(bh[-t.id,]$MEDV, predict(mod, newdata = bh[-t.id,]),
     xlab = "Actual", ylab = "Predichos")
abline(0,1)

#mtry = m/3, donde m = # de predictores
#nodesize = 5 - por defecto un nodo terminal necesita tener al menos 5 elementos (esto se puede cambiar)
#maxnodes 