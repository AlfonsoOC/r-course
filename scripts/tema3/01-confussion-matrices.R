library(tidyverse)
cp <- read.csv("r-course-master/data/tema3/college-perf.csv")

# asignar niveles, es decir que Low vale menos que Medium y este vale menos que High
cp$Perf <- ordered(cp$Perf, 
                   levels = c("Low", "Medium", "High"))
cp$Pred <- ordered(cp$Pred,
                   levels = c("Low", "Medium", "High"))

#TABLAS DE FRECUENCIA o Matrices de confucion 
table <- table(cp$Perf, cp$Pred, 
               dnn =  c("Actual", "Predecido"))
table

#tabla de frecuencia en porcentaje (todos los parametros son el 100%)
prop.table(table)

#da una tabla de frecuencias en porcentaje tomando el 100% por fila
round(prop.table(table, 1)*100, 2)

#con 100% con las columnas
round(prop.table(table, 2)*100, 2)


#visualizacion de tablas de frecuencia con DIAGRAMAS DE MOSAICO
barplot(table, legend = TRUE, 
        xlab = "Nota predecida por el modelo")

#MOSAICO
mosaicplot(table, main = "Eficiencia del modelo")

#da las estadisticas de las tablas de frecuencia 
summary(table)
# aqui un valor bajo de la P-value significa que las proporciones para las diferentes 
#clases son singnificativamente diferente y que por lo tanto estan relacionadas 
#es decir que no son independientes, en este caso "la eficiencia del modelo es buena" 
