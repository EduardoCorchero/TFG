install.packages('sae')
library(sae)

#Los datos que voy usar corresponden a datos sintéticos sobre los ingresos y otras variables relacionadas
#en las provincias de España.
data=data(incomedata)
head(data)


#Primero voy a realizar el preprocesamiento de los datos en Jupyter.
#Para ello voy a exportar el dataframe incomedata:
#write.csv(incomedata, "incomedata.csv") #punto y coma como separador y coma como separador decimal.

#PREPROCESAMIENTO Y ANÁLISIS EXPLORATORIO HECHO EN JUPYTER, ANEXO, APLICACIÓN EN SOFTWARE 1.PREPROCESING.ipynb.

#Ya tengo el dataframe preprocesado: 'incomedata_procesado'
attach(incomedata_procesado)
head(incomedata_procesado)
summary(incomedata_procesado)

#Ahora voy a convertir las variables categóricas en factores y actualizar el conjunto de datos con los factores:
provf <- factor(prov) #No lo voy a usar para construir el árbol, porque se produce el siguiente error:
#Error in tree(income ~ ., data = incomedata_procesado_f.train) : 
#factor predictors must have at most 32 levels
acf <- factor(ac)
genf <- factor(gen)
natf <- factor(nat)
laborf <- factor(labor)
age2f <- factor(age2)
age3f <- factor(age3)
age4f <- factor(age4)
age5f <- factor(age5)
educ1f <- factor(educ1)
educ2f <- factor(educ2)
educ3f <- factor(educ3)

incomedata_procesado <- data.frame(prov, acf, genf, natf, laborf, age2f, age3f, age4f, age5f, educ1f, educ2f, educ3f, income)
head(incomedata_procesado)
summary(incomedata_procesado)

################################################
#Primero vamos a construir un ÁRBOL DE REGRESIÓN: 
################################################

#Importamos las librerías necesarias.
library(ISLR)
library(MASS)
library(tree)

# 1: Dividimos el conjunto de datos en un conjunto de entrenamiento y un conjunto de prueba.
set.seed(15)
train <- sample(1:nrow(incomedata_procesado), nrow(incomedata_procesado) / (4/3)) #75% de datos de entrenamiento
incomedata_procesado.train <- incomedata_procesado[train, ]
incomedata_procesado.test <- incomedata_procesado[-train, ]

# b: Ajustamos un árbol de regresión al conjunto de entrenamiento. 
# Graficamos el árbol e interpretamos resultados. 
tree.incomedata_procesado <- tree(income ~ ., data = incomedata_procesado.train)
summary(tree.incomedata_procesado) 
#El árbol ajustado tiene tres nodos terminales,
#se han usado las variables relacionadas con la educación y la situación laboral, como ya preveíamos.
#Nos sorprende que la variable provincia no aparezca en el árbol ya que observábamos cierta dependencia en los gráficos.
plot(tree.incomedata_procesado)
text(tree.incomedata_procesado, pretty = 0)
#Veamos ahora el error cuadrático medio de test:
yhat <- predict(tree.incomedata_procesado, newdata = incomedata_procesado.test)
mean((yhat - incomedata_procesado.test$income)^2) #ECM de test =  44985824

# c: Utilizamos la validación cruzada para determinar el nivel óptimo de árbol: complejidad. 
cv.incomedata_procesado <- cv.tree(tree.incomedata_procesado)
cv.incomedata_procesado$size  # Tamaños de los subárboles podados
cv.incomedata_procesado$dev   # SCE de los subárboles podados
cv.incomedata_procesado$k     # Secuencia de valores por defecto de la penalización por el tamaño del árbol en la poda
plot(cv.incomedata_procesado$size, cv.incomedata_procesado$dev, type = "b")
tree.min <- which.min(cv.incomedata_procesado$dev)
tree.min
points(cv.incomedata_procesado$size[tree.min], cv.incomedata_procesado$dev[tree.min], col = "red", cex = 2, pch = 20)
#La poda del árbol no mejora la tasa de error de la prueba, nos quedamos con el árbol con 3 nodos terminales.

SCT <- var(incomedata_procesado$income) #Suma de Cuadrado Total(SCT)
yhat <- predict(tree.incomedata_procesado, newdata = incomedata_procesado)
SCE <- mean((yhat - mean(yhat))^2) #SCE
SCR <- SCT-SCE #Suma de cuadrados debido al modelo
R <- SCR/SCT
R #El porcentaje de variabilidad explicada por el modelo es del 88,53%, por lo que el modelo se puede considerar bueno para predecir.

###################################################
#Ahora vamos a construir un ÁRBOL DE CLASIFICACIÓN:
###################################################

#Como valores ilustrativos,según la información que proporciona la Encuesta de Condiciones de Vida del año 2021, 
#el valor del umbral de riesgo de pobreza de un hogar en España de una sola persona 
#(calculado con los datos de ingresos de 2020) se situó en 9.535 euros anuales

rp <- factor(ifelse(income<=9535,"Sí","No")) #Creamos el factor rp (riesgo de pobreza)
incomedata_procesado_cl=data.frame(incomedata_procesado,rp)
summary(incomedata_procesado_cl) #vemos que hay 7233 personas en riesgo de pobreza.
#Construimos el árbol y lo graficamos:
tree.incomedata_procesado_cl=tree(rp~.-income,incomedata_procesado_cl)
summary(tree.incomedata_procesado_cl)
#En este caso el árbol ajustado tiene 3 nodos terminales, y ultiliza solo dos variables, los mismas que en regresión.
#El error de clasificación de entrenamiento  es del 39.62%, por lo que el modelo no es muy preciso
plot(tree.incomedata_procesado_cl)
text(tree.incomedata_procesado_cl,pretty=0)
tree.incomedata_procesado_cl

#Para evaluar correctamente el rendimiento de un árbol de clasificación en estos datos, 
#debemos estimar el error de prueba en lugar de simplemente calcular el error de entrenamiento. 
#Dividimos las observaciones en un conjunto de entrenamiento y un conjunto de prueba, 
#construimos el árbol utilizando el conjunto de entrenamiento y evaluamos su rendimiento en el conjunto de prueba.

set.seed(25)
train <- sample(1:nrow(incomedata_procesado_cl), nrow(incomedata_procesado_cl) / (4/3)) #75% de datos de entrenamiento
incomedata_procesado_cl.test=incomedata_procesado_cl[-train, ]
rp.test <- rp[-train]
tree.incomedata_procesado_cl=tree(rp~.-income,incomedata_procesado_cl,subset=train)
tree.pred <- predict(tree.incomedata_procesado_cl,incomedata_procesado_cl.test,type="class")
tree.pred
table(tree.pred,rp.test)
1 - (1696+875)/4300 #el error de test es 0.4023256

#A continuación, examinamos si la poda del árbol puede mejorar los resultados:
cv.incomedata_procesado_cl=cv.tree(tree.incomedata_procesado_cl,FUN=prune.misclass)
cv.incomedata_procesado_cl
par(mfrow=c(1,2))
plot(cv.incomedata_procesado_cl$size,cv.incomedata_procesado_cl$dev,type="b")
plot(cv.incomedata_procesado_cl$k,cv.incomedata_procesado_cl$dev,type="b")
#En este caso, La poda del árbol no mejora la tasa de error de la prueba, 
#nos quedamos con el árbol con 3 nodos terminales.
prune.incomedata_procesado_cl=prune.misclass(tree.incomedata_procesado_cl,best=3)
plot(prune.incomedata_procesado_cl)
text(prune.incomedata_procesado_cl,pretty=0)


####################
#BAGGING (regresión)
####################

library(randomForest)
bag.incomedata_procesado=randomForest(income~.,data=incomedata_procesado,subset=train,mtry=12,importance=TRUE)
# Al querer hacer el modelo bagging, se deben considerar las 12 variables predictoras para cada división del árbol, por ello el argumento mtry = 12. 
bag.incomedata_procesado  

#Veamos cuál es el porcentaje de variabilidad explicada por el modelo:
SCT <- var(incomedata_procesado$income) #Suma de Cuadrado Total(SCT)
yhat.bag <- predict(bag.incomedata_procesado,newdata=incomedata_procesado[-train,])
SCE.bag <- mean((yhat.bag - mean(yhat.bag))^2) #SCE
SCR.bag <- SCT-SCE.bag #Suma de cuadrados debido al modelo
R.bag <- SCR.bag/SCT
R.bag #El porcentaje de variabilidad explicada por el modelo es del 73,11%, por lo que el modelo se puede considerar bueno para predecir.

#Veamos ahora el error cuadrático de test:
yhat.bag = predict(bag.incomedata_procesado,newdata=incomedata_procesado[-train,])
plot(yhat.bag, incomedata_procesado[-train,]$income)
abline(0,1)
mean((yhat.bag-incomedata_procesado[-train,]$income)^2)
#El ECM de test asociado al árbol de regresión de Bagging es de 47502657, 
#superando el obtenido utilizando un árbol único podado de forma óptima. 
#En este sentido, el primer árbol de regresión es mejor que el del modelo Bagging.
#Veamos ahora si el modelo mejora, si en vez de 500 árboles, utilizamos 1000 para su construcción.
bag.incomedata_procesado <- randomForest(income~.,data=incomedata_procesado,subset=train,mtry=12,ntree=1000)
bag.incomedata_procesado #La variabilidad del modelo no mejora sustancialmente
yhat.bag = predict(bag.incomedata_procesado,newdata=incomedata_procesado[-train,])
mean((yhat.bag-incomedata_procesado[-train,]$income)^2) #El ECM de test no mejora sustancialmente.


##############
#RANDOM FOREST 
##############

#Para la construcción del modelo de Random Forest vamos a considerar 3 variables en cada partición (aproximación de la raíz de 12)
rf.incomedata_procesado=randomForest(income~.,data=incomedata_procesado,subset=train,mtry=3,importance=TRUE)
rf.incomedata_procesado

yhat.rf = predict(rf.incomedata_procesado,newdata=incomedata_procesado[-train,])
mean((yhat.rf-incomedata_procesado.test$income)^2)
#El ECM de test (43836636) es mejor que el obtenido utilizando un árbol único podado de forma óptima

#Veamos cuál es el porcentaje de variabilidad explicada por el modelo:
SCT <- var(incomedata_procesado$income) #Suma de Cuadrado Total(SCT)
SCE.rf <- mean((yhat.rf - mean(yhat.rf))^2) #SCE
SCR.rf <- SCT-SCE.rf #Suma de cuadrados debido al modelo
R.rf <- SCR.rf/SCT
R.rf #El porcentaje de variabilidad explicada por el modelo es del 84,17%, por lo que el modelo se puede considerar bueno para predecir.

#A continuación se presentan dos medidas de importancia de las variables. 
#La primera se basa en la disminución media de la precisión de las predicciones en la muestra de test
#cuando se permuta una variable determinada. 
#La segunda es una medida de la disminución total en la impureza de los nodos que resulta de las divisiones sobre esa variable, 
#promediada sobre  todos los árboles. 
#En el caso de los árboles de regresión, la impureza de nodos se mide por el SCE de entrenamiento, 
#y para los árboles de clasificación por la desviación.
importance(rf.incomedata_procesado) 
varImpPlot(rf.incomedata_procesado) 
#Como ya preveíamos, las variables más importantes del modelo son 'educ3' y 'labor',
#la variable provincia también aparece como una variable importante en el modelo.


#########
#BOOSTING
#########

#Aquí utilizamos el paquete gbm, y dentro de él la función gbm(),
#para ajustar árboles de regresión Boosting a nuestro conjunto de datos. 
library(gbm)

#Ejecutamos gbm() con la opción distribución = "gaussian", ya que se trata de un problema de regresión.
#Para un problema de clasificación binaria utilizaríamos distribución = "bernoulli".
#El argumento n.trees = 5000 indica que queremos 5000 árboles, y la opción
#opción interaction.depth = 4 limita la profundidad de cada árbol.
boost.incomedata_procesado=gbm(income~.,data=incomedata_procesado[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
boost.incomedata_procesado
summary(boost.incomedata_procesado)
#Vemos que prov y ac son las variables más importantes del Boosting. También lo son las variables educ3 y labor, como en los anteriores modelos.

#Veamos ahora las predicciones del modelo, y el error cuadrático medio de test:
yhat.boost=predict(boost.incomedata_procesado,newdata=incomedata_procesado[-train,],n.trees=5000)
mean((yhat.boost-incomedata_procesado[-train,]$income)^2) #El ECM obtenido (44987846) es mejor que los demás modelos derivados, pero peor que el del primer árbol de regresión.

#Veamos cuál es el porcentaje de variabilidad explicada por el modelo:
SCT <- var(incomedata_procesado$income) #Suma de Cuadrado Total(SCT)
SCE.boost <- mean((yhat.boost - mean(yhat.boost))^2) #SCE
SCR.boost <- SCT-SCE.boost #Suma de cuadrados debido al modelo
R.boost <- SCR.boost/SCT
R.boost #El porcentaje de variabilidad explicada por el modelo es del 77'5%, por lo que el modelo se puede considerar bueno para predecir.

#Ahora realizaremos el boosting con un valor del parámetro de contracción λ distinto,para ver si el ECM mejora.
#El valor por defecto es 0,001 pero tomaremos λ = 0.15.
boost.incomedata_procesado=gbm(income~.,data=incomedata_procesado[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.15,verbose=F)
yhat.boost=predict(boost.incomedata_procesado,newdata=incomedata_procesado[-train,],n.trees=5000)
mean((yhat.boost-incomedata_procesado[-train,]$income)^2) #El ECM no mejora sustancialmente.
#Se podría hacer cross-validation para la obtención del mejor hiperparámetro que minimice el ECM

