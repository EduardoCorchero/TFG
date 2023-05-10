############################################################
#ESTIMACIÓN EN ÁREAS PEQUEÑAS UTILIZANDO ÁRBOLES DE DECISIÓN
############################################################

#############################################################################
# Lectura de datos
#############################################################################

#Estimación de condiciones de vida (2006)

ecv0106recod<-read.table("DatosECV0106_Recodif.txt",header=TRUE)
ecv0106recod[1:10,]
attach(ecv0106recod)
nECV<-dim(ecv0106recod)[1]

#############################################################################
# Estudiamos la distribución de probabilidad de la variable rentanorm
#############################################################################

# Comprobamos si hace falta alguna transformación de la variable 
# para poder asumir normalidad.

rentanormt<-rentanorm+abs(min(rentanorm))+900
y<-log(rentanormt)
hist(rentanorm,prob=TRUE,main="",xlab="rentanorm",ylab="",xlim=c(0,100000))
hist(y,prob=TRUE,breaks=20,main="",xlab="log-rentanorm",ylab="",xlim=c(8,12))
x<-seq(from=8,to=12,by=0.1)
lines(x,dnorm(x,mean(log(rentanormt)),sqrt(var(log(rentanormt)))))

# El logaritmo de rentanorm tiene distribuciónn aproximadamente normal, 
# por lo que usaremos la variable de la renta transformada 'y'.


##############################################################################
# Selección de variables explicativas para la estimación de la pobreza
#############################################################################

# Hacemos un análisis descriptivo para estudiar las relaciones 
# entre las variables explicativas y la renta normalizada.

# Primero miramos si la renta normalizada presenta diferencias por ccaa
ymeanccaa<-tapply(y,ccaa,mean)
plot(1:18,ymeanccaa,type="b")
anova(lm(y~as.factor(ccaa)))
#Sale muy significativo: Sí hay diferencias de renta por ccaa

# ----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: y
# Df Sum Sq Mean Sq F value    Pr(>F)    
# as.factor(ccaa)    17  339.4    20.0  128.41 < 2.2e-16 ***
#   Residuals       34371 5344.1     0.2                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# ----------------------------------------------------------------

#Miramos ahora si la renta normalizada presenta diferencias por prov
ymeanprov<-tapply(y,prov,mean) 
plot(1:52,ymeanprov,type="b")
anova(lm(y~as.factor(prov)))
# También sale muy significativo: Sí hay diferencias por prov.
# ----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: y
# Df Sum Sq Mean Sq F value    Pr(>F)    
# as.factor(prov)    51  408.0     8.0  52.063 < 2.2e-16 ***
#   Residuals       34337 5275.6     0.2                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# ----------------------------------------------------------------

#Ahora miramos si hay diferencias por dominios=prov x sex
ymeandom<-tapply(y,dom,mean) 
plot(1:104,ymeandom,type="b")
anova(lm(y~as.factor(dom)))
#También sale muy significativo: Sí hay diferencias por dominios
# ----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: y
# Df Sum Sq Mean Sq F value    Pr(>F)    
# as.factor(dom)   103  419.9     4.1  26.553 < 2.2e-16 ***
#   Residuals      34285 5263.6     0.2                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# ----------------------------------------------------------------

#Ahora miramos si hay diferencias por sexo, edad, nacio, educ, situa:

#Diferencias por sexo:

tapply(rentanorm,sexo,mean)
# ----------------------------------------------------------------
#   1        2
# 12734.69 12244.11
# ----------------------------------------------------------------

tapply(y,sexo,mean)
# ----------------------------------------------------------------
#   1        2
# 9.711572 9.683099
# ----------------------------------------------------------------

anova(lm(rentanorm~as.factor(sexo)))
# ----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: rentanorm
# Df     Sum Sq    Mean Sq F value    Pr(>F)
# as.factor(sexo)     1 2.0746e+09 2.0746e+09  32.365 1.288e-08 ***
#   Residuals       34513 2.2123e+12 6.4101e+07
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ----------------------------------------------------------------

anova(lm(y~as.factor(sexo)))
# ----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: y
# Df Sum Sq Mean Sq F value    Pr(>F)    
# as.factor(sexo)     1    7.0     7.0  42.302 7.929e-11 ***
#   Residuals       34387 5676.5     0.2                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# ----------------------------------------------------------------

#Diferencias por edad:

tapply(rentanorm,edad,mean)
# ----------------------------------------------------------------
#   0        1        2        3        4        5
# 12024.39 10315.24 11812.69 13607.91 13373.93 10247.09
# ----------------------------------------------------------------

tapply(y,edad,mean)
# ----------------------------------------------------------------
#   0        1        2        3        4        5
# 9.663193 9.576767 9.663689 9.757443 9.743916 9.584115
# ----------------------------------------------------------------

anova(lm(rentanorm~as.factor(edad)))
# ----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: rentanorm
# Df     Sum Sq    Mean Sq F value    Pr(>F)
# as.factor(edad)     5 5.7223e+10 1.1445e+10  183.08 < 2.2e-16 ***
#   Residuals       34509 2.1572e+12 6.2511e+07
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ----------------------------------------------------------------

anova(lm(y~as.factor(edad)))
# -----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: y
# Df Sum Sq Mean Sq F value    Pr(>F)
# as.factor(edad)     5  157.2    31.4  195.46 < 2.2e-16 ***
#   Residuals       34509 5551.5     0.2
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ----------------------------------------------------------------

#Diferencias por nacionalidad: 

tapply(rentanorm,nacio,mean)
# ----------------------------------------------------------------
#   0         1
# 9615.537 12602.845
# ----------------------------------------------------------------

tapply(y,nacio,mean)
# ----------------------------------------------------------------
#   0        1
# 9.528768 9.703995
# ----------------------------------------------------------------

anova(lm(rentanorm~as.factor(nacio)))
# ----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: rentanorm
# Df     Sum Sq    Mean Sq F value    Pr(>F)
# as.factor(nacio)     1 1.1987e+10 1.1987e+10  187.84 < 2.2e-16 ***
#   Residuals        34513 2.2024e+12 6.3814e+07
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ----------------------------------------------------------------

anova(lm(y~as.factor(nacio)))
# --------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: y
# Df Sum Sq Mean Sq F value    Pr(>F)
# as.factor(nacio)     1   41.2    41.2  251.16 < 2.2e-16 ***
#   Residuals        34513 5667.4     0.2
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ----------------------------------------------------------------

#Diferencias por el nivel de educación:

tapply(rentanorm,educ,mean)
# ----------------------------------------------------------------
#   0        1        2        3
# 11993.78  9858.45 12731.25 19271.20
# ----------------------------------------------------------------

tapply(y,educ,mean)
# ----------------------------------------------------------------
#   0         1         2         3
# 9.665952  9.559880  9.721894 10.022312
# ----------------------------------------------------------------

anova(lm(rentanorm~as.factor(educ)))
# ----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: rentanorm
# Df     Sum Sq    Mean Sq F value    Pr(>F)
# as.factor(educ)     3 2.6708e+11 8.9026e+10  1579.1 < 2.2e-16 ***
#   Residuals       34472 1.9435e+12 5.6379e+07
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ----------------------------------------------------------------

anova(lm(y~as.factor(educ)))
# ----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: y
# Df Sum Sq Mean Sq F value    Pr(>F)
# as.factor(educ)     3  654.5   218.2  1490.9 < 2.2e-16 ***
#   Residuals       34472 5044.0     0.1
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ----------------------------------------------------------------

#Diferencias por la situación laboral:

tapply(rentanorm,situa,mean)
# ----------------------------------------------------------------
#   0         1         2         3
# 11993.784 14770.634  9786.045 10651.839
# ----------------------------------------------------------------

tapply(y,situa,mean)
# ----------------------------------------------------------------
#   0        1        2        3
# 9.665952 9.822119 9.539465 9.599894
# ----------------------------------------------------------------

anova(lm(rentanorm~as.factor(situa)))
# ----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: rentanorm
# Df     Sum Sq    Mean Sq F value    Pr(>F)
# as.factor(situa)     3 1.2589e+11 4.1962e+10  693.28 < 2.2e-16 ***
#   Residuals        34424 2.0836e+12 6.0526e+07
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ----------------------------------------------------------------

anova(lm(y~as.factor(situa)))
# ----------------------------------------------------------------
#   Analysis of Variance Table
# 
# Response: y
# Df Sum Sq Mean Sq F value    Pr(>F)
# as.factor(situa)     3  374.2   124.7  807.23 < 2.2e-16 ***
#   Residuals        34424 5319.5     0.2
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ----------------------------------------------------------------


########################################
#CONSTRUCCIÓN DEL ÁRBOL DE DECISIÓN
########################################

# Ajustamos un árbol de decisión para la variables rentanorm 
# con las variables explicativas significativas:
# (a) edad2, edad3, edad4 y edad5: quitamos edad1 (referencia) para que no haya colinearidad
# (b) nacio1: Quitamos nacio2 (referencia)
# (c) edu1 y edu3: Quitamos edu0 (que es igual a edad1) y edu2 (estudios secundarios, categ. de referencia). 
# Si no quitásemos edu2, entonces edu1+edu2+edu3 sumarían lo mismo que edad2+edad3+edad4+edad5.
# (d) situ 1 y situ 2: Quitamos situ0 (que es igual a edad1) y situ 3 (Inactivos, categ. de referencia).


datos<-read.table("DatosECV0106_Recodif.txt",header=TRUE)
datos[1:10,]
attach(datos)

#Para construir el árbol de decisión, vamos a usar los datos de 'ecv0106recod'
#como si fuese la población sobre la que queremos hacer el estudio y modelo de la renta.

#Primero vamos a calcular la media de la renta poblacional real en cada comunidad autónoma:
renta_media <- c()
for(d in 1:18){
  renta_media_d <- mean(datos[ccaa==d,'rentanorm'], na.rm = TRUE)
  renta_media <- c(renta_media,renta_media_d)
}
renta_media

#Calculamos ahora la incidencia de pobreza poblacional real en cada comunidad.
#El umbral de pobreza se ha calculado de antemano como el 60% de la mediana de los ingresos, 
#y resulta ser z=6557.143. 
incidencia_pobreza <- c()
for(d in 1:18){
  incidencia_pobreza_d <- mean(datos[ccaa==d,'rentanorm']<6557.143, na.rm = TRUE)
  incidencia_pobreza <- c(incidencia_pobreza,incidencia_pobreza_d)
}
incidencia_pobreza

#Primero vemos el tamaño poblacional de cada comunidad autónoma:
frec_ccaa <- as.data.frame(table(ccaa))
frec_ccaa

#Para sacar una muestra de nuestra población, vamos a hacer un muestreo estratificado con afijación proporcional
#con m.a.s en cada comunidad autónoma. 
#Para determinar el tamaño muestral, vamos a calcular el peso de cada comunidad
#de forma que el tamaño muestral en la comunidad más pequeña no sea inferior a 20.
#La 'comunidad autónoma' con menor población (829) es Ceuta y Melilla.
#Para obtener una muestra de, al menos, tamaño 20, nos quedaremos con el 2'5% de los datos como muestra.

# #Procedemos a realizar el muestreo:
# set.seed(33)
# s <- c()
# count2 <- 0
# for(d in 1:18){
#   s_d <- sample(1:frec_ccaa[d,'Freq'], size=round(0.025*frec_ccaa[d,'Freq']), replace=F) + count2
#   s <- c(s,s_d)
#   count2 <- count2 + frec_ccaa[d,'Freq']
# }
# s
# 
# muest <- datos[s,] #muestra con todas las variables originales
# fuera_muest <- datos[-s, -c(26)] #observaciones fuera de la muestra con todas las variables originales, sin la dependiente 'y'
# 
# write.csv(muest, "muest.csv") 
# write.csv(fuera_muest, "fuera_muest.csv") 

#Como cada vez que ejecutamos el bucle anterior nos sale una muestra distinta, y por tanto nos saldrá un
#árbol distinto para cada muestra, generamos el fichero muest.csv y fuera_muest.csv 
#para quedarnos con una única muestra.

muest <- read.table("muest.csv", header=TRUE, sep=',')
fuera_muest <- read.table("fuera_muest.csv", header=TRUE, sep=',')

#Procedemos a la construcción del árbol de decisión con nuestra muestra.

#Para la construcción del árbol de decisión utilizaremos las variables que han resultado significativas
#en el estudio anterior: edad2,edad3,edad4,edad5,nacio1,edu1,edu3,situ1,situ2,
#y la variable ccaa, corespondiente a las comunidades autónomas

#muestra solo con las variables del modelo
muestra <- muest[,c('ccaa','edad2','edad3','edad4','edad5','edu1','edu3','nacio1','situ1','situ2','rentanorm')] 
#observaciones fuera de la muestra solo con las variables del modelo, sin la variable dependiente 'y'
fuera_muestra <- fuera_muest[,c('ccaa','edad2','edad3','edad4','edad5','edu1','edu3','nacio1','situ1','situ2')]

#Importamos las librerías necesarias.
library(ISLR)
library(MASS)
library(tree)

# 1: Consideramos la muestra como el conjunto de datos de entrenamiento.
# Posteriormente simularemos distintas muestras de test para evaluar el ECM y el sesgo del árbol.
muestra.train <- muestra

# b: Ajustamos un árbol de regresión al conjunto de entrenamiento. 
# Graficamos el árbol e interpretamos resultados. 
tree.muestra <- tree(rentanorm ~ ., data = muestra.train)
summary(tree.muestra) 
#El árbol ajustado tiene 6 nodos terminales,
#Se han usado las variables 'edu3', 'situ1', 'edu1', 'edad4'.
#Nos sorprende que la variable ccaa no aparezca en el árbol ya que observábamos diferencias significativas entre las medias de la renta.
plot(tree.muestra)
text(tree.muestra, pretty = 0)

# c: Utilizamos la validación cruzada para determinar el nivel óptimo de árbol: complejidad. 
cv.muestra <- cv.tree(tree.muestra)
cv.muestra$size  # Tamaños de los subárboles podados
cv.muestra$dev   # SCE de los subárboles podados
cv.muestra$k     # Secuencia de valores por defecto de la penalización por el tamaño del árbol en la poda
plot(cv.muestra$size, cv.muestra$dev, type = "b")
tree.min <- which.min(cv.muestra$dev)
tree.min
points(cv.muestra$size[tree.min], cv.muestra$dev[tree.min], col = "red", cex = 2, pch = 20)
#La poda del árbol mejora el SCE del modelo, nos quedamos con el árbol con 5 nodos terminales.
prune.muestra=prune.tree(tree.muestra,best=5)
plot(prune.muestra)
text(prune.muestra,pretty=0)
tree.muestra <- prune.muestra
summary(tree.muestra) 

SCT <- var(muestra$rentanorm) #Suma de Cuadrado Total(SCT)
mediahat <- predict(tree.muestra, newdata = muestra)
SCE <- mean((mediahat - mean(mediahat))^2) #SCE
SCR <- SCT-SCE #Suma de cuadrados debido al modelo
R <- SCR/SCT
R #El porcentaje de variabilidad explicada por el modelo es del 84,49%, por lo que el modelo se puede considerar bueno para predecir.


#Ahora vamos a estimar la media de la renta y la incidencia de pobreza en cada comunidad autónoma, 
#así como el ECM y el sesgo de cada estimador, a través de la simulación de 200 submuestras de test.

est_media_s <- c()
est_inc_s <- c()

for (i in 1:200){
set.seed(33+i)
s <- c()
count <- 0
for(d in 1:18){
    s_i <- sample((1:frec_ccaa[d,'Freq']), size=round(0.025*frec_ccaa[d,'Freq']), replace=F) + count
    s <- c(s,s_i)
    count <- count + frec_ccaa[d,'Freq']
  }
test <- datos[s,c('ccaa','edad2','edad3','edad4','edad5','edu1','edu3','nacio1','situ1','situ2','rentanorm')] #muestra de test con las variables del modelo

#Estimador de la media:
est_media <- c()
for(d in 1:18){
est_media_d <- mean(c(muestra[muestra$ccaa==d, 11], predict(tree.muestra, newdata = test[test$ccaa==d, ])))
est_media <- c(est_media,est_media_d)
}
est_media_s <- c(est_media_s, est_media)

#Estimador de la incidenacia de pobreza:
est_inc <- c()
for(d in 1:18){
  est_inc_d <- mean(c((muestra[muestra$ccaa==d, 11]<6557.143), (predict(tree.muestra, newdata = test[test$ccaa==d, ])<6557.143)))
  est_inc <- c(est_inc,est_inc_d)
}
est_inc_s <- c(est_inc_s, est_inc)

}

#Veamos ahora los estimadores de la simulación por ccaa
est_media_ccaa <- c()
for (d in 1:18){
  est_media_ccaa_d <- sum(est_media_s[seq(d,length(est_media_s),12)])/200
  est_media_ccaa <- c(est_media_ccaa, est_media_ccaa_d)
}
est_media_ccaa #estimador de la simulación de la media de ingresos por ccaa
#18386.65 17793.55 20509.10 19549.98 17484.43 17367.87 18399.91 17799.39 20490.18 19544.92 17477.22
#17367.36 18330.32 17729.73 20447.02 19486.19 17424.23 17304.09

est_inc_ccaa <- c()
for (d in 1:18){
  est_inc_ccaa_d <- sum(est_inc_s[seq(d,length(est_inc_s),12)])/200
  est_inc_ccaa <- c(est_inc_ccaa, est_inc_ccaa_d)
}
#0.14736237 0.18630383 0.08914835 0.10113979 0.23395945 0.18450635 0.14736237 0.18630383 0.08914835
#0.10113979 0.23395945 0.18450635 0.14671505 0.18597488 0.08864835 0.10074917 0.23314550 0.18393817est_inc_ccaa #estimador de la simulación de la incidencia de pobreza por ccaa


#Para evaluar la precisión del árbol, compararemos los estimadores anteriores con los valores reales, 
#a través del error cuadrático medio (ECM) de cada estimador por comunidades:

ECM_media_ccaa <- c()
for (d in 1:18){
  ECM_media_ccaa_d <- mean((est_media_s[seq(d,length(est_media_s),12)] - renta_media)^2)
  ECM_media_ccaa <- c(ECM_media_ccaa, ECM_media_ccaa_d)
}
ECM_media_ccaa #ECM del estimador por simulación de la media de ingresos por ccaa
#6689212 3676404 7397782 4832321 5652990 3902860 3482013 4889807 3835455 3069969 4847483 4970008
#3919945 3389412 5778036 3147901 3437879 4337101

ECM_inc_ccaa <- c()
for (d in 1:18){
  ECM_inc_ccaa_d <- mean((est_inc_s[seq(d,length(est_inc_s),12)] - incidencia_pobreza)^2)
  ECM_inc_ccaa <- c(ECM_inc_ccaa, ECM_inc_ccaa_d)
}
ECM_inc_ccaa #ECM del estimador por simulación de la incidencia de pobreza por ccaa
#0.021655541 0.012873347 0.031139918 0.027075861 0.014336877 0.012297117 0.017521005 0.018485687
#0.026714501 0.025237976 0.010858847 0.015220304 0.018578470 0.014559221 0.028971894 0.025210846
#0.008705574 0.016491007

#Calculamos ahora SESGO de cada estimador por comunidades:

sesgo_media_ccaa <- est_media_ccaa - renta_media
sesgo_media_ccaa #Sesgo del estimador de la media de ingresos por ccaa
#7959.968 4356.827 7355.611 4912.363 5984.217 4257.930 6756.786 6941.332 5952.557 7264.761 7579.891
#5833.323 3341.117 6887.054 4406.591 4474.464 5241.649 5662.425

sesgo_inc_ccaa <- est_inc_ccaa - incidencia_pobreza
sesgo_inc_ccaa #Sesgo del estimador de la incidencia de pobreza por ccaa
#-0.151031921  0.055232637 -0.032271325 -0.023367692 -0.028797147  0.063399087 -0.092086381
#-0.108684783 -0.030238286 -0.059691715 -0.120234097 -0.039590032  0.020722460 -0.087471447
#-0.027486400 -0.004339326  0.010378079 -0.170705978

#El estimador de la media sobreestima significativamente el nivel de renta.
#El estimador de la incidencia de pobreza infraestima este parámetro, lo cual tiene sentido al sobreestimar el valor de la renta


#Vamos a construir ahora el modelo mejor predictor empírico bajo el modelo de errores anidados,
#para compararlo posteriormente con los estimadores del árbol.


###############################################################
#MEJOR PREDICTOR EMPÍRICO BAJO EL MODELO DE ERRORES ANIDADOS
###############################################################

library(sae)

n<-dim(muestra)[1] # Tamaño muestral total
D<-length(unique(ccaa)) # Número de provincias (áreas o dominios)
nd<-as.vector(table(ccaa)) # Tamaños muestrales de las provincias
Nd<-frec_ccaa$Freq # Tamaños poblacionales de las provincias


# Estimadores EB de las INCIDENCIAS DE POBREZA y la MEDIA DE INGRESOS. 

#El umbral de pobreza se ha calculado de antemano como el 60% de la mediana de los ingresos, y resulta ser 
# z=6557.143. Usando este umbral, necesitamos definir la función que nos da la incidencia de pobreza:

povertyincidence <- function(y) {
  result <- mean(y < 6557.143)
  return (result)
}

# Ahora llamamos a la función que calcula los estimadores EB seleccionando como indicador dicha función
# povertyincidence, tomando transformación logaritmo (por defecto) y añadiendo la constante
# constant=abs(min(rentanorm))+900 a los ingresos antes de dicha transformación, y utilizando repeticiones para la
# aproximación de Monte Carlo de los estimadores EB. La constante mencionada se selecciona de manera
# que los residuos del ajuste muestren una distribución aproximadamente simétrica, ya que el método EB
# descrito se basa en la distribución normal. Antes de llamar a la función, fijamos la semilla de los
# generadores de números aleatorios para que la función nos proporcione las mismas estimaciones en el caso
# de repetir la llamada a esta función, e inicializamos el vector que contendrá a los estimadores EB. :

m <- abs(min(rentanorm))+900

#Estimador INCIDENCIA POBREZA
ccaa.EB<-numeric(D)
set.seed(123) # Fijamos la semilla para números aleatorios
res.EB<-ebBHF(rentanorm~edad2+edad3+edad4+edad5+nacio1+edu1+edu3+situ1+situ2,dom=ccaa,
               Xnonsample=fuera_muestra,MC=50,constant=m,indicator=povertyincidence)
ccaa.EB<-res.EB$eb$eb
ccaa.EB
#0.24314137 0.11895553 0.10996847 0.10924182 0.22057024 0.11580607 0.19010903 0.23826990 0.10295023
#0.14502678 0.29131003 0.17803579 0.11548110 0.22298055 0.09445242 0.09426491 0.18579235 0.27213195

#Estimador MEDIA INGRESOS
ccaa.EB_mean<-numeric(D)
set.seed(123) # Fijamos la semilla para números aleatorios
res.EB_mean<-ebBHF(rentanorm~edad2+edad3+edad4+edad5+nacio1+edu1+edu3+situ1+situ2,dom=ccaa,
                   Xnonsample=fuera_muestra,MC=50,constant=m,indicator=mean)
ccaa.EB_mean<-res.EB_mean$eb$eb
ccaa.EB_mean
#11515.74 14342.74 14388.37 15132.39 12194.99 14054.76 12803.36 11827.32 15390.14 13323.74 10751.74
#12927.70 15131.63 12011.85 16521.67 15559.70 12987.27 12103.73

# Para cualquier modelo, conviene analizar los residuos para comprobar que los datos no presenten
# evidencias claras en contra del modelo asumido. Dado que el método EB requiere normalidad,
# representamos un histograma y un gráfico q-q de normalidad de los residuos: 

resid.EB<-res.EB$fit$residuals
hist(resid.EB,main="",xlab="Residuals")
qqnorm(resid.EB,main="")

resid.EB_mean<-res.EB$fit$residuals
hist(resid.EB_mean,main="",xlab="Residuals")
qqnorm(resid.EB_mean,main="")

# Ambos gráficos muestran que la distribución de los residuos es aproximadamente
# normal. Por el contrario, si ajustamos el modelo a los ingresos sin la transformación logaritmo, tanto el
# histograma como el gráfico q-q de normalidad (no se incluyen por brevedad) muestran una distribución
# marcadamente asimétrica a la derecha. Por tanto, dicha transformación es necesaria para no alejarnos de
# la hipótesis de normalidad.

# Finalmente, calculamos los estimadores Bootstrap del ECM de los estimadores EB con B=200
# iteraciones Bootstrap y MC=50 iteraciones para la aproximación Monte Carlo de los estimadores EB. 

#ECM Estimador INCIDENCIA POBREZA
set.seed(123)
ccaa.mse.res<-
  pbmseebBHF(rentanorm~edad2+edad3+edad4+edad5+nacio1+edu1+edu3+situ1+situ2,dom=ccaa,
             Xnonsample=fuera_muestra,B=200,MC=50,constant=m,indicator=povertyincidence)

ccaa.EB.mse<-numeric(D)
ccaa.EB.mse<-ccaa.mse.res$mse$mse
ccaa.EB.mse
#1.760708e-05 2.890231e-05 3.860364e-05 3.666686e-05 3.015935e-05 6.034031e-05 1.918641e-05
#3.071206e-05 1.985482e-05 2.479731e-05 3.418015e-05 2.152228e-05 3.759754e-05 2.770803e-05
#5.231231e-05 3.563692e-05 4.096447e-05 6.040397e-05

#ECM Estimador MEDIA INGRESOS
set.seed(123)
ccaa.mse_mean.res<-
  pbmseebBHF(rentanorm~edad2+edad3+edad4+edad5+nacio1+edu1+edu3+situ1+situ2,dom=ccaa,
             Xnonsample=fuera_muestra,B=200,MC=50,constant=m,indicator=mean)

ccaa.EB.mse_mean<-numeric(D)
ccaa.EB.mse_mean<-ccaa.mse_mean.res$mse$mse
ccaa.EB.mse_mean
#13525.30 19338.34 26496.43 25721.01 22039.92 38090.50 16291.54 22902.34 15697.28 16324.19 26526.23
#18708.65 22886.74 20489.65 28819.46 25730.73 27682.85 42032.74

#Los ECM anteriores no están calculados de la misma forma que los ECM a partir del árbol, 
#y por tanto no pueden ser comparados. 
#Para comparar la precisión de los modelos calcularemos al final del script
#las diferencias simples con los valores reales, a través de una muestra de test.


#Ahora vamos a simular 200 muestras de test para calcular los estimadores de la media y la incidencias,
#a través la media de los estimadores calculados en cada iteración.

estEB_media_s <- c()
estEB_inc_s <- c()

for (i in 1:200){
  set.seed(54+i)
  s <- c()
  count <- 0
  for(d in 1:18){
    s_i <- sample((1:frec_ccaa[d,'Freq']), size=round(0.025*frec_ccaa[d,'Freq']), replace=F) + count
    s <- c(s,s_i)
    count <- count + frec_ccaa[d,'Freq']
  }
  test <- datos[s,c('ccaa','edad2','edad3','edad4','edad5','edu1','edu3','nacio1','situ1','situ2')] #muestra de test con las variables del modelo

  res.EB_mean_d<-ebBHF(rentanorm~edad2+edad3+edad4+edad5+nacio1+edu1+edu3+situ1+situ2,dom=ccaa,
                       Xnonsample=test,MC=50,constant=m,indicator=mean)
  #Estimador de la media:
  estEB_media <- c()
  for(d in 1:18){
    estEB_media_d <- mean(c(muestra[muestra$ccaa==d, 11], res.EB_mean$eb$eb))
    estEB_media <- c(estEB_media,estEB_media_d)
  }
  estEB_media_s <- c(estEB_media_s, estEB_media)
  
  res.EB_d<-ebBHF(rentanorm~edad2+edad3+edad4+edad5+nacio1+edu1+edu3+situ1+situ2,dom=ccaa,
                Xnonsample=test,MC=50,constant=m,indicator=povertyincidence)
  #Estimador de la incidenacia de pobreza:
  estEB_inc <- c()
  for(d in 1:18){
    estEB_inc_d <- mean(c((muestra[muestra$ccaa==d, 11]<6557.143), res.EB_d$eb$eb))
    estEB_inc <- c(estEB_inc,estEB_inc_d)
  }
  estEB_inc_s <- c(estEB_inc_s, estEB_inc)
  
}

#Veamos ahora los estimadores de la simulación por ccaa:

estEB_media_ccaa <- c()
for (d in 1:18){
  estEB_media_ccaa_d <- sum(estEB_media_s[seq(d,length(estEB_media_s),12)])/200
  estEB_media_ccaa <- c(estEB_media_ccaa, estEB_media_ccaa_d)
}
estEB_media_ccaa #estimador de la simulación de la media de ingresos por ccaa
#18328.40 17809.60 21228.20 20007.61 17357.64 17587.14 18328.40 17809.60 21228.20 20007.61 17357.64
#17587.14 18272.90 17745.61 21165.44 19943.76 17298.21 17525.44

estEB_inc_ccaa <- c()
for (d in 1:18){
  estEB_inc_ccaa_d <- sum(estEB_inc_s[seq(d,length(estEB_inc_s),12)])/200
  estEB_inc_ccaa <- c(estEB_inc_ccaa, estEB_inc_ccaa_d)
}
estEB_inc_ccaa #estimador de la simulación de la incidencia de pobreza por ccaa
#0.3011138 0.3530964 0.2174560 0.2307015 0.4167674 0.3352815 0.3011153 0.3530990 0.2174605 0.2307052
#0.4167707 0.3352872 0.2998592 0.3523268 0.2164544 0.2298396 0.4153232 0.3342041


#Para evaluar la precisión del modelo BP, compararemos los estimadores anteriores con los valores reales, 
#a través del error cuadrático medio (ECM) de cada estimador por comunidades:

ECMEB_media_ccaa <- c()
for (d in 1:18){
  ECMEB_media_ccaa_d <- mean((estEB_media_s[seq(d,length(estEB_media_s),12)] - renta_media)^2)
  ECMEB_media_ccaa <- c(ECMEB_media_ccaa, ECMEB_media_ccaa_d)
}
ECMEB_media_ccaa #ECM del estimador por simulación de la media de ingresos por ccaa
#7777567 3686037 8694887 5777003 6493316 3798418 3821516 5131533 4987548 3500390 5373608 4774992
#4446234 3324766 7277445 3628380 3621643 3590582

ECMEB_inc_ccaa <- c()
for (d in 1:18){
  ECMEB_inc_ccaa_d <- mean((estEB_inc_s[seq(d,length(estEB_inc_s),12)] - incidencia_pobreza)^2)
  ECMEB_inc_ccaa <- c(ECMEB_inc_ccaa, ECMEB_inc_ccaa_d)
}
ECMEB_inc_ccaa #ECM del estimador por simulación de la incidencia de pobreza por ccaa
#0.013377486 0.008567404 0.014659625 0.011946753 0.020562976 0.006378941 0.006970912 0.016417890
#0.009300056 0.009147544 0.015672130 0.009518235 0.008868333 0.011032904 0.012995476 0.009469837
#0.012872549 0.011222891


#Calculamos ahora SESGO de cada estimador por comunidades:

sesgoEB_media_ccaa <- estEB_media_ccaa - renta_media
sesgoEB_media_ccaa #Sesgo del estimador de la media de ingresos por ccaa
#7901.719 4372.877 8074.707 5370.002 5857.436 4477.203 6685.280 6951.541 6690.571 7727.455 7460.318
#6053.107 3283.698 6902.933 5125.010 4932.037 5115.633 5883.775

sesgoEB_inc_ccaa <- estEB_inc_ccaa - incidencia_pobreza
sesgoEB_inc_ccaa #Sesgo del estimador de la incidencia de pobreza por ccaa
#0.002719481  0.222025186  0.096036342  0.106194023  0.154010814  0.214174264  0.061666504
#0.058110438  0.098073840  0.069873646  0.062577171  0.111190851  0.173866595  0.078880470
#0.100319604  0.124751085  0.192555789 -0.020440031




###################################################
#MODELO BP CON LAS VARIABLES UTILIZADAS EN EL ÁRBOL
###################################################

#El árbol da una idea de qué variables pueden tener interacciones entre sí. 
#Método de selección de variables, no como modelo final.

fuera_muestra_b <- fuera_muestra[,c('ccaa','edu1','edu3', 'situ1')]
fuera_muestra_b <- cbind(fuera_muestra_b, fuera_muestra$edu3*fuera_muestra$situ1, fuera_muestra$situ1*fuera_muestra$edu1)

#Estimador INCIDENCIA POBREZA
ccaa.EB_b<-numeric(D)
set.seed(123) # Fijamos la semilla para números aleatorios
res.EB_b<-ebBHF(rentanorm~edu1+edu3+situ1+edu3*situ1+situ1*edu1,dom=ccaa,
                     Xnonsample=fuera_muestra_b,MC=50,constant=m,indicator=povertyincidence)
ccaa.EB_b<-res.EB_b$eb$eb
ccaa.EB_b
#0.2965831 0.1454987 0.1447604 0.1254988 0.2577903 0.1449883 0.2387920 0.2860669 0.1250076 0.1791210
#0.3430056 0.2315250 0.1273117 0.2737757 0.1080162 0.1093307 0.2151316 0.3146610

#Estimador MEDIA INGRESOS
ccaa.EB_mean_b<-numeric(D)
set.seed(123) # Fijamos la semilla para números aleatorios
res.EB_mean_b<-ebBHF(rentanorm~edu1+edu3+situ1+edu3*situ1+situ1*edu1,dom=ccaa,
                   Xnonsample=fuera_muestra_b,MC=50,constant=m,indicator=mean)
ccaa.EB_mean_b<-res.EB_mean_b$eb$eb
ccaa.EB_mean_b
#10438.556 13476.083 13205.935 14537.526 11352.522 13147.604 11606.995 10803.584 14532.456 12356.340
#9887.076 11543.848 14856.150 10799.753 15915.098 15036.087 12206.597 11236.119

# Para cualquier modelo, conviene analizar los residuos para comprobar que los datos no presenten
# evidencias claras en contra del modelo asumido. Dado que el método EB requiere normalidad,
# representamos un histograma y un gráfico q-q de normalidad de los residuos: 

resid.EB_b<-res.EB_mean_b$fit$residuals
hist(resid.EB_b,main="",xlab="Residuals")
qqnorm(resid.EB_b,main="")

resid.EB_mean_b<-res.EB_b$fit$residuals
hist(resid.EB_mean_b,main="",xlab="Residuals")
qqnorm(resid.EB_mean_b,main="")

# Ambos gráficos muestran que la distribución de los residuos es aproximadamente
# normal. Por el contrario, si ajustamos el modelo a los ingresos sin la transformación logaritmo, tanto el
# histograma como el gráfico q-q de normalidad (no se incluyen por brevedad) muestran una distribución
# marcadamente asimétrica a la derecha. Por tanto, dicha transformación es necesaria para no alejarnos de
# la hipótesis de normalidad.

# Finalmente, calculamos los estimadores Bootstrap del ECM de los estimadores EB con B=200
# iteraciones Bootstrap y MC=50 iteraciones para la aproximación Monte Carlo de los estimadores EB. 

#ECM Estimador INCIDENCIA POBREZA
set.seed(123)
ccaa.mse.res_b<-
  pbmseebBHF(rentanorm~edu1+edu3+situ1+edu3*situ1+situ1*edu1,dom=ccaa,
             Xnonsample=fuera_muestra_b,B=200,MC=50,constant=m,indicator=povertyincidence)

ccaa.EB.mse_b<-numeric(D)
ccaa.EB.mse_b<-ccaa.mse.res_b$mse$mse
ccaa.EB.mse_b
#1.288283e-05 3.781415e-05 4.215847e-05 3.285375e-05 3.182603e-05 6.143000e-05 2.380553e-05
#2.814940e-05 1.393260e-05 2.061944e-05 3.302926e-05 2.511170e-05 3.000344e-05 3.826106e-05
#5.128637e-05 2.877004e-05 4.315470e-05 6.781127e-05

#ECM Estimador MEDIA INGRESOS
set.seed(123)
ccaa.mse_mean.res_b<-
  pbmseebBHF(rentanorm~edu1+edu3+situ1+edu3*situ1+situ1*edu1,dom=ccaa,
             Xnonsample=fuera_muestra_b,B=200,MC=50,constant=m,indicator=mean)

ccaa.EB.mse_mean_b<-numeric(D)
ccaa.EB.mse_mean_b<-ccaa.mse_mean.res_b$mse$mse
ccaa.EB.mse_mean_b
#3704.628 12313.098 14307.041 18205.608 12848.253 24008.499  8552.054 11859.093  6586.290  8468.181
#13979.763  8231.223 14676.413 11428.210 19082.551 12421.159 18727.817 28927.125

#Comparamos el ECM de cada estimador, entre el modelo BP con las variables significativas y 
#el modelo BP con las variables utilizadas en el árbol.

ccaa.EB.mse_mean_b - ccaa.EB.mse_mean
#-9814.429  -7020.725 -12161.865  -7474.532  -9167.660 -14031.377  -7718.615 -11027.333  -9126.018
#-7836.129 -12487.391 -10465.626  -8242.550  -9036.319  -9738.206 -13316.567  -8943.156 -13120.278

ccaa.EB.mse_b - ccaa.EB.mse
# 4.882558e-06  8.005454e-06  2.561091e-06 -4.439373e-06  2.471109e-06  1.624477e-06  4.810879e-06
# -2.806478e-06 -5.566210e-06 -4.387559e-06 -1.704732e-06  3.580790e-06 -8.212307e-06  9.400087e-06
# -9.210658e-07 -6.724156e-06  2.689386e-06  8.878860e-06

#Observamos que en ambos estimadores se consigue un menor ECM en muchas de las ccaa,
#utilizando el modelo BP con las variables utilizadas en el árbol.


#Ahora vamos a simular 200 muestras de test para calcular los estimadores de la media y la incidencias,
#a través la media de los estimadores calculados en cada iteración.

estEB_media_s_b <- c()
estEB_inc_s_b <- c()

for (i in 1:200){
  set.seed(59+i)
  s <- c()
  count <- 0
  for(d in 1:18){
    s_i <- sample((1:frec_ccaa[d,'Freq']), size=round(0.025*frec_ccaa[d,'Freq']), replace=F) + count
    s <- c(s,s_i)
    count <- count + frec_ccaa[d,'Freq']
  }
  test_b <- datos[s,c('ccaa','edu1','edu3', 'situ1')]
  test_b <- cbind(test_b, test_b$edu3*test_b$situ1, test_b$situ1*test_b$edu1)
  
  res.EB_mean_d<-ebBHF(rentanorm~edu1+edu3+situ1+edu3*situ1+situ1*edu1,dom=ccaa,
                Xnonsample=test_b,MC=50,constant=m,indicator=mean)
  #Estimador de la media:
  estEB_media_b <- c()
  for(d in 1:18){
    estEB_media_d_b <- mean(c(muestra[muestra$ccaa==d, 11], res.EB_mean_d$eb$eb))
    estEB_media_b <- c(estEB_media_b,estEB_media_d_b)
  }
  estEB_media_s_b <- c(estEB_media_s_b, estEB_media_b)
  
  res.EB_d<-ebBHF(rentanorm~edu1+edu3+situ1+edu3*situ1+situ1*edu1,dom=ccaa,
                     Xnonsample=test_b,MC=50,constant=m,indicator=povertyincidence)
  #Estimador de la incidenacia de pobreza:
  estEB_inc_b <- c()
  for(d in 1:18){
    estEB_inc_d_b <- mean(c((muestra[muestra$ccaa==d, 11]<6557.143), res.EB_d$eb$eb))
    estEB_inc_b <- c(estEB_inc,estEB_inc_d)
  }
  estEB_inc_s_b <- c(estEB_inc_s_b, estEB_inc_b)
  
}

#Veamos ahora los estimadores de la simulación por ccaa
estEB_media_ccaa_b <- c()
for (d in 1:18){
  estEB_media_ccaa_d_b <- sum(estEB_media_s_b[seq(d,length(estEB_media_s_b),12)])/200
  estEB_media_ccaa_b <- c(estEB_media_ccaa_b, estEB_media_ccaa_d_b)
}
estEB_media_ccaa_b #estimador de la simulación de la media de ingresos por ccaa
#18315.99 17792.44 21211.73 19991.41 17338.06 17565.54 18315.96 17792.38 21211.63 19991.33 17337.99
#17565.42 18260.51 17728.50 21149.02 19927.62 17278.68 17503.92

estEB_inc_ccaa_b <- c()
for (d in 1:18){
  estEB_inc_ccaa_d_b <- sum(estEB_inc_s_b[seq(d,length(estEB_inc_s_b),12)])/200
  estEB_inc_ccaa_b <- c(estEB_inc_ccaa_b, estEB_inc_ccaa_d_b)
}
estEB_inc_ccaa_b #estimador de la simulación de la incidencia de pobreza por ccaa
#0.3324329 0.3321857 0.3318278 0.3311458 0.3321729 0.3329621 0.3323537 0.3327789 0.3309384 0.3303964
#0.3313626 0.3316195 0.3311781 0.3314157 0.3308258 0.3302835 0.3307283 0.3318841


#Para evaluar la precisión del modelo, compararemos los estimadores anteriores con los valores reales, 
#a través del error cuadrático medio (ECM) de cada estimador por comunidades:

ECMEB_media_ccaa_b <- c()
for (d in 1:18){
  ECMEB_media_ccaa_d_b <- mean((estEB_media_s_b[seq(d,length(estEB_media_s_b),12)] - renta_media)^2)
  ECMEB_media_ccaa_b <- c(ECMEB_media_ccaa_b, ECMEB_media_ccaa_d_b)
}
ECMEB_media_ccaa_b #ECM del estimador por simulación de la media de ingresos por ccaa
#7777274 3703270 8653733 5761175 6515459 3818602 3827164 5147426 4950829 3481701 5401410 4797045
#4447066 3342259 7246564 3616715 3648082 3622104

ECMEB_inc_ccaa_b <- c()
for (d in 1:18){
  ECMEB_inc_ccaa_d_b <- mean((estEB_inc_s_b[seq(d,length(estEB_inc_s_b),12)] - incidencia_pobreza)^2)
  ECMEB_inc_ccaa_b <- c(ECMEB_inc_ccaa_b, ECMEB_inc_ccaa_d_b)
}
ECMEB_inc_ccaa_b #ECM del estimador por simulación de la incidencia de pobreza por ccaa
#0.01163121 0.01167473 0.01148020 0.01165473 0.01162382 0.01165853 0.01179323 0.01170704 0.01122152
#0.01150142 0.01196451 0.01163055 0.01172039 0.01152474 0.01163488 0.01125894 0.01152175 0.01178622

#Calculamos ahora SESGO de cada estimador por comunidades:

sesgoEB_media_ccaa_b <- estEB_media_ccaa_b - renta_media
sesgoEB_media_ccaa_b #Sesgo del estimador de la media de ingresos por ccaa
#7889.306 4355.715 8058.237 5353.793 5837.853 4455.606 6672.834 6934.322 6674.005 7711.167 7440.664
#6031.386 3271.309 6885.827 5108.594 4915.891 5096.101 5862.256

sesgoEB_inc_ccaa_b <- estEB_inc_ccaa_b - incidencia_pobreza
sesgoEB_inc_ccaa_b #Sesgo del estimador de la incidencia de pobreza por ccaa
#0.03403859  0.20111448  0.21040814  0.20663831  0.06941627  0.21185478  0.09290497  0.03779026
#0.21155176  0.16956491 -0.02283094  0.10752309  0.20518556  0.05796941  0.21469106  0.22519498
#0.10796092 -0.02276001


plot(ECMEB_media_ccaa, type = "l", col = "blue", ylab = "ECM", xlab = "ccaa")
lines(ECMEB_media_ccaa_b, col = "red")
legend("topright", legend = c("EB_var_signif", "EB_var_arbol"), col = c("blue", "red"), lty = 1)
title(main = "ECM estimador renta media")

plot(ECMEB_inc_ccaa, type = "l", col = "blue", ylab = "ECM", xlab = "ccaa")
lines(ECMEB_inc_ccaa_b, col = "red")
legend("topright", legend = c("EB_var_signif", "EB_var_arbol"), col = c("blue", "red"), lty = 1)
title(main = "ECM estimador incidencia")

plot(sesgoEB_media_ccaa, type = "l", col = "blue", ylab = "sesgo", xlab = "ccaa")
lines(sesgoEB_media_ccaa_b, col = "red")
legend("topright", legend = c("EB_var_signif", "EB_var_arbol"), col = c("blue", "red"), lty = 1)
title(main = "Sesgo estimador renta media")

plot(sesgoEB_inc_ccaa, type = "l", col = "blue", ylab = "sesgo", xlab = "ccaa")
lines(sesgoEB_inc_ccaa_b, col = "red")
legend("bottomleft", legend = c("EB_var_signif", "EB_var_arbol"), col = c("blue", "red"), lty = 1)
title(main = "Sesgo estimador incidencia")


#Por último, vamos a ver como se comportan los tres modelos para una muestra en concreto

#########################################
#ESTIMADORES PARA UNA MUESTRA EN CONCRETO
#########################################

#  set.seed(35)
#  s_test <- c()
#  count3 <- 0
#  for(d in 1:18){
#    s_test_d <- sample(1:frec_ccaa[d,'Freq'], size=round(0.025*frec_ccaa[d,'Freq']), replace=F) + count3
#    s_test <- c(s_test,s_test_d)
#    count3 <- count3 + frec_ccaa[d,'Freq']
# }
# s_test
#  
# muest_test <- datos[s_test,] #muestra con todas las variables originales
#  
# write.csv(muest_test, "muest_test.csv") 

#Como cada vez que ejecutamos el bucle anterior nos sale una muestra distinta, y por tanto nos saldrá un
#árbol distinto para cada muestra, generamos el fichero muest.csv y fuera_muest.csv 
#para quedarnos con una única muestra.

muest_test <- read.table("muest_test.csv", header=TRUE, sep=',')
muestra_test <- muest_test[,c('ccaa','edad2','edad3','edad4','edad5','edu1','edu3','nacio1','situ1','situ2','rentanorm')]
muestra_test_EB <- muest_test[,c('ccaa','edad2','edad3','edad4','edad5','edu1','edu3','nacio1','situ1','situ2')]
muestra_test_EB_b <- muestra_test[,c('ccaa','edu1','edu3', 'situ1')]
muestra_test_EB_b <- cbind(muestra_test_EB_b, muestra_test_EB_b$edu3*muestra_test_EB_b$situ1, muestra_test_EB_b$situ1*muestra_test_EB_b$edu1)


#Estimadores del árbol de decisión podado de forma óptima: 

est_inc_test <- c()
for(d in 1:18){
  est_inc_test_d <- mean(c((muestra_test[muestra_test$ccaa==d, 11]<6557.143), (predict(tree.muestra, newdata = muestra_test[muestra_test$ccaa==d, ])<6557.143)))
  est_inc_test <- c(est_inc_test,est_inc_test_d)
}
est_inc_test
#0.14732143 0.03947368 0.01250000 0.07812500 0.17441860 0.09090909 0.08620690 0.13636364 0.07692308
#0.08695652 0.16666667 0.12903226 0.05319149 0.14772727 0.03571429 0.05555556 0.06000000 0.09523810

est_media_test <- c()
for(d in 1:18){
  est_media_test_d <- mean(c(muestra_test[muestra_test$ccaa==d, 11], predict(tree.muestra, newdata = muestra_test[muestra_test$ccaa==d, ])))
  est_media_test <- c(est_media_test,est_media_test_d)
}
est_media_test
#11372.71 13734.90 13204.79 14288.58 11685.03 12232.19 12128.74 11651.96 13275.60 12373.23 11413.45
#11524.19 13958.81 11515.85 14946.39 14570.98 13261.45 12450.78


#Estimadores del modelo BP con las variables significativas

ccaa.EB_test<-numeric(D)
set.seed(123) # Fijamos la semilla para números aleatorios
res.EB_test<-ebBHF(rentanorm~edad2+edad3+edad4+edad5+nacio1+edu1+edu3+situ1+situ2,dom=ccaa,
              Xnonsample=muestra_test_EB,MC=50,constant=m,indicator=povertyincidence)
ccaa.EB_test<-res.EB_test$eb$eb
ccaa.EB_test
#0.2957789 0.1310448 0.1217861 0.1236895 0.2610755 0.1205174 0.2371092 0.2922556 0.1185252 0.1602917
#0.3511517 0.2218025 0.1260537 0.2706395 0.1151557 0.1046411 0.2218774 0.3501412

ccaa.EB_mean_test<-numeric(D)
set.seed(123) # Fijamos la semilla para números aleatorios
res.EB_mean_test<-ebBHF(rentanorm~edad2+edad3+edad4+edad5+nacio1+edu1+edu3+situ1+situ2,dom=ccaa,
                   Xnonsample=muestra_test_EB,MC=50,constant=m,indicator=mean)
ccaa.EB_mean_test<-res.EB_mean_test$eb$eb
ccaa.EB_mean_test
#10476.881 13461.778 13187.970 14653.533 11522.409 13158.414 11702.372 10914.294 14584.234 12325.417
#9935.715 11597.766 14976.296 10913.957 16041.407 15042.299 12195.114 11660.293


#Estimadores del modelo BP con las variables utilizadas en el árbol

ccaa.EB_test_b<-numeric(D)
set.seed(123) # Fijamos la semilla para números aleatorios
res.EB_test_b<-ebBHF(rentanorm~edu1+edu3+situ1+edu3*situ1+situ1*edu1,dom=ccaa,
                Xnonsample=muestra_test_EB_b,MC=50,constant=m,indicator=povertyincidence)
ccaa.EB_test_b<-res.EB_test_b$eb$eb
ccaa.EB_test_b
#0.2981593 0.1319014 0.1222357 0.1240430 0.2628604 0.1228571 0.2397899 0.2954667 0.1196420 0.1618499
#0.3531026 0.2245376 0.1262603 0.2735061 0.1152941 0.1053859 0.2218008 0.3529882

ccaa.EB_mean_test_b<-numeric(D)
set.seed(123) # Fijamos la semilla para números aleatorios
res.EB_mean_test_b<-ebBHF(rentanorm~edu1+edu3+situ1+edu3*situ1+situ1*edu1,dom=ccaa,
                     Xnonsample=muestra_test_EB_b,MC=50,constant=m,indicator=mean)
ccaa.EB_mean_test_b<-res.EB_mean_test_b$eb$eb
ccaa.EB_mean_test_b
#10427.230 13437.790 13178.363 14649.161 11485.160 13090.005 11635.655 10847.436 14532.017 12279.302
#9909.308 11531.805 14980.865 10834.709 16064.022 15015.780 12201.124 11611.209


plot(est_inc_test, type = "l", col = "blue", ylim = c(0, 0.37), ylab = "incidencia", xlab = "ccaa")
lines(ccaa.EB_test, col = "red")
lines(ccaa.EB_test_b, col = "green")
lines(incidencia_pobreza, col = "black")
legend("topleft", legend = c("arbol", "EB_var_signif", "EB_var_arbol", "valor_real"), col = c("blue", "red", "green", "black"), lty = 1)
title(main = "Estimadores incidencia .vs. incidencia real")

plot(est_media_test, type = "l", col = "blue", ylim = c(10000, 16000), ylab = "renta", xlab = "ccaa")
lines(ccaa.EB_mean_test, col = "red")
lines(ccaa.EB_mean_test_b, col = "green")
lines(renta_media, col = "black")
legend("topleft", legend = c("arbol", "EB_var_signif", "EB_var_arbol", "valor_real"), col = c("blue", "red", "green", "black"), lty = 1)
title(main = "Estimadores renta .vs. renta media real")


