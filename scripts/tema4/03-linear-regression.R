#linear regression 

library(caret) #caret es para realizar la particion de los datos
auto <- read.csv("../../data/tema4/auto-mpg.csv")
auto$cylinders <- factor(auto$cylinders,
                         levels = c(3,4,5,6,8),
                         labels = c("3c", "4c", "5c", "6c", "8c"))

set.seed(2018)
t.id <- createDataPartition(auto$mpg, p = 0.7, list = F) #treaining id
names(auto)

#aqui se hace la regresion lineal
mod <- lm(mpg ~ ., #regresion lineal para predecir mpg en funcion del resto de variables del dataset
          data = auto[t.id,-c(1,8,9)]) # utilizando los datos de entrenamiento pero quitando las columnas 1, 8 y 9

mod

#mpg = 38.607312 +
#     + 7.212652*4c + 5.610350*5c + 3.307172*6c + 6.211343*8c +
#     + 0.006878 * displacement - 0.072209 * horsepower +
#     - 0.005156 * weight + 0.024852 * acceleration

summary(mod)

# el residuo nos da el error
boxplot(mod$residuals)

sqrt(mean((mod$fitted.values - auto[t.id,]$mpg)^2))

#para evaluar la prediccion con el conjunto de validacion hacemos o siguiente
pred <- predict(mod, auto[-t.id, -c(1,8,9)])
sqrt(mean((pred - auto[-t.id,]$mpg)^2))

#para visulizar nuestro modelo
par(mfrow=c(2,2))
plot(mod)
#la explicacion de los graficos es la siguiente
# 1.- primer grafico la linea roja tiene que ser cercana a una recta para demostrar que los datos tenian una relacion lineal
# 2.- grafico quantil-quantil, esta si sigue una linea recta significa que siguen una dstribucion normal, la cual es necesaria para usar regresion lineal 
# 3.- nos habla de la escala y localizacion de los reciduos, aqui el grafico debe parecer horizontal 
# 4.- reciduos contra apalancamiento, los outliers muchas veces son influyentes en la regresion lineal (si su influencia es mucha aveces es mejor quitarlo)


#aqui estoy cambiando los niveles de referencias, por default tratara de usar el primero, en este caso 
#3c pero esos carros son muy pocos, asi que puedo cambiar ese posible problema especificando que 
#quiero que el nivel de 4c sea mi nivel de referencia
auto <- within(auto, 
               cylinders <- relevel(cylinders, ref="4c"))
mod <- lm(mpg ~. , data = auto[t.id, -c(1,8,9)])
summary(mod)
pred <- predict(mod, auto[-t.id, -c(1,8,9)])
sqrt(mean((pred-auto[-t.id,]$mpg)^2))
plot(mod)

#cargar el paquete MASS 
library(MASS)

#una vez que ya se tiene un modelo "mod" de regresion lineal 
mod
summary(mod)

#creamos un objeto "step model" 
#buscar que variables deben formar parte o no del modelo
step.model <- stepAIC(mod, direction="backward") #backward quita variables apartir de un modelo completo
#podemos usar "forward" que pone variables a un modelo incompleto
summary(step.model)

#en esta parte el modelo se refina para utilizar menos variables que no aortan mucho 
