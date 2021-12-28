dat <- read.csv("../../data/tema4/rmse.csv")

# esto nos ayuda a calcular el error cuadratico medio de estos datos que son numericos entre datos reales y datos predichos

rmse<-sqrt(mean((dat$price - dat$pred)^2)) 
rmse

#el error es de 2.9 pero eso no nos dice nada realmente, hay que ver si ese 
#error es grande o pequenio dependiendo de los datos que tenemos, entonces para visualizar ese error
# realizamos un plot de los datos reales contra los predichos y observamos un comportamiento 
#muy bueno en el que tenemos una linea de ~45 grados
plot(dat$price, dat$pred, xlab = "Actual", ylab="Predicho")
abline(0,1)

#esta es una funcion que nos permite calcular el error cuadratico medio
rmse <- function(actual, predicted){
  return(sqrt(mean((actual-predicted)^2)))
}

rmse(dat$price, dat$pred)
