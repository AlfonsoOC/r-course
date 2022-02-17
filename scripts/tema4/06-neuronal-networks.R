library(nnet)
library(caret)
library(devtools)

bh <- read.csv("~/Documents/GitHub/r-course/data/tema4/BostonHousing.csv")

set.seed(2018)
t.id <- createDataPartition(bh$MEDV, p= 0.7, list = F)
summary(bh$MEDV)

#A continuacion creare la red neuronal usando la funcion "nnet"
#en este caso como el maximo es 50 lo que hare sera escalar todo dividido entre 50, esto hace al modelo mejor (no estoy seguro por que)
fit <- nnet(MEDV/50 ~., data=bh[t.id, ],
            size = 6, decay = 0.1,
            maxit = 1000, linout=T)

#cargo una funcion desde una direccion de internet
#fawda 123 neuronal network
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")

#Max.sp = maximum space: es para qu e grafique los puntos lo mas espaciados posible
plot(fit, max.sp = T)

#calcular el error cuadratico medio
sqrt(mean((fit$fitted.values*50-bh[t.id,"MEDV"])^2))


pred <- predict(fit, bh[-t.id,])
sqrt(mean((pred*50 -  bh[-t.id,"MEDV"])^2))
