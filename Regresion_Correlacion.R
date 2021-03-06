#######################################################
# UNIVERSIDAD NACIONAL DE INGENIER�A
# CENTRO DE TECNOLOG�AS DE INFORMACI�N Y COMUNICACIONES
# Paper: Regresi�n y Correlaci�n 
# Elaborado: Abraham Zamudio
# Codificaci�n por: Victor Lazo
#########################################################

getwd()
setwd(dir = "c:/Users/Victor Lazo/Desktop/R4DS/05 Class/Tareas/")
rm(list=ls())

#### Regresi�n y Correlaci�n ####

#### Regresi�n lineal simple ####

# Ejemplo 13
# edad: edad de una mujer
# presion: presi�n sanguinea
a�os <- c(3, 2, 2, 4, 3, 5) 
sueldo <- c(5, 5, 4, 9, 7, 11)
summary(a�os)
summary(sueldo)

# Sea la variable dependiente sueldo y la variable independiente a�os
plot(a�os, sueldo)

# Ajuste del modelo lineal
reg_lin <- lm(sueldo ~ a�os)
reg_lin

# Call:
#   lm(formula = sueldo ~ a�os)
# 
# Coefficients:
#   (Intercept)         a�os  
# -0.1951       2.2195  

# donde obtenemos el modelo lineal: 
# \beta_{0} = -0.1951, \beta_{1} = 2.2195  
# sueldo: \beta_{0} + \beta_{1}*a�os  

# Datos de la regresi�n
summary(reg_lin)
# Call:
#   lm(formula = sueldo ~ a�os)
# 
# Residuals:
#   1        2        3        4        5        6 
# -1.46341  0.75610 -0.24390  0.31707  0.53659  0.09756 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  -0.1951     1.1381  -0.171  0.87220   
# a�os          2.2195     0.3406   6.517  0.00286 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8903 on 4 degrees of freedom
# Multiple R-squared:  0.9139,	Adjusted R-squared:  0.8924 
# F-statistic: 42.47 on 1 and 4 DF,  p-value: 0.002863


# dibujamos la recta de regresi�n lineal
x11()
plot(a�os ~ sueldo)
abline(reg_lin)

# Agregamos algunas caracteristicas al codigo
summary(reg_lin)
xmin <- 0.9*min(a�os)
xmax <- 1.1*max(a�os)
ymin <- 0.9*min(sueldo)
ymax <- 1.1*max(sueldo)
plot(a�os, sueldo, main="sueldo ~ a�os", sub="Pob.: 6 personas",
     xlab = "a�os", ylab = "sueldo", 
     xlim = c(xmin, xmax), ylim = c(ymin,ymax))
abline(reg_lin)
dev.off()

# test de normalidad de Kolmogorov-Smirnov
ks.test(reg_lin$residuals, "pnorm")

# test de Durbin-Watson
library(lmtest)
dwtest(sueldo ~ a�os)

# Durbin-Watson test

# data:  sueldo ~ a�os
# DW = 2.0443, p-value = 0.4372
# alternative hypothesis: true autocorrelation is greater than 0
# En este caso, con un p-valor = 0.4372 no podemos rechazar la
# hip�tesis de que los residuos son independientes.


#### Metodo de minimos cuadrados ####

# Ejemplo 21

# x: tiempo en centurias
# y: distancia entre dos lenguas

x <- c(1:8)
y <- c(1, 2, 1, 2, 3, 2, 2, 2 )

x11()
plot(y ~ x)

reg_lin <- lm(y ~ x)
reg_lin
abline(reg_lin)
summary(reg_lin)

# Ejemplo 35 #
rm(list = ls())

#Datos por pares (x,y)
x <- c(1,2,1,3,4,4)
y <- c(2,1,3,3,3,2)
# El s�mbolo ~ indica que se estudia una relaci�n entre
# la variable dependiente a la izquierda y las independientes
# a la derecha.
# La regresi�n se estudia por medio de
# los modelos lineales (linear models).
# mod es el nombre del modelo a estudiar.
mod<-lm(y ~ x)
summary(mod)

# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5       6 
# -0.1754 -1.2807  0.8246  0.6140  0.5088 -0.4912 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   2.0702     0.8157   2.538   0.0641 .
# x             0.1053     0.2915   0.361   0.7362  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8983 on 4 degrees of freedom
# Multiple R-squared:  0.03158,	Adjusted R-squared:  -0.2105 
# F-statistic: 0.1304 on 1 and 4 DF,  p-value: 0.7362

# Ejercicio 36 #

rm(list = ls())
#Datos por pares (x,y)
x <- c(1, 1, 2, 3, 4)
y <- c(2, 3, 5, 5, 9)
# El s�mbolo ~ indica que se estudia una relaci�n entre
# la variable dependiente a la izquierda y las independientes
# a la derecha.
# La regresi�n se estudia por medio de
# los modelos lineales (linear models).

# mod es el nombre del modelo a estudiar.
mod <- lm(y ~ x)

# Liste los coeficientes de regresi�n

mod$coefficients
summary(mod)

# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5 
# -0.4706  0.5294  0.5882 -1.3529  0.7059 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   0.5294     0.9825   0.539   0.6274  
# x             1.9412     0.3946   4.919   0.0161 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.029 on 3 degrees of freedom
# Multiple R-squared:  0.8897,	Adjusted R-squared:  0.8529 
# F-statistic:  24.2 on 1 and 3 DF,  p-value: 0.01609

# Ejercicio 37 #

#Limpia la memoria
rm(list = ls())
#Datos por pares (x,y)
x <- c(1, 2, 3, 4, 5, 6, 7)
y <- c(2, 5, 8, 9, 8, 5, 2)
modeloLineal <- lm(y ~ x)
summary(modeloLineal)

# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5       6       7 
# -3.5714 -0.5714  2.4286  3.4286  2.4286 -0.5714 -3.5714 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) 5.571e+00  2.665e+00   2.091   0.0908 .
# x           4.511e-16  5.959e-01   0.000   1.0000  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.153 on 5 degrees of freedom
# Multiple R-squared:  1.111e-31,	Adjusted R-squared:   -0.2 
# F-statistic: 5.554e-31 on 1 and 5 DF,  p-value: 1

# Los datos no se ajusta a una l�nea, probaremos que se ajusta una par�bola
+ rm(list = ls())
#Datos por pares (x,y)
x <- c(1, 2, 3, 4, 5, 6, 7)
y <- c(2, 5, 8, 9, 8, 5, 2)
modeloparabolico <- lm(y ~ poly(x, 2, raw = TRUE))
summary(modeloparabolico)

# Call:
#   lm(formula = y ~ poly(x, 2, raw = TRUE))
# 
# Residuals:
#   1       2       3       4       5       6       7 
# 0.2381 -0.5714  0.1429  0.3810  0.1429 -0.5714  0.2381 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             -3.57143    0.76042  -4.697 0.009331 ** 
#   poly(x, 2, raw = TRUE)1  6.09524    0.43579  13.987 0.000152 ***
#   poly(x, 2, raw = TRUE)2 -0.76190    0.05324 -14.311 0.000139 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.488 on 4 degrees of freedom
# Multiple R-squared:  0.9808,	Adjusted R-squared:  0.9713 
# F-statistic: 102.4 on 2 and 4 DF,  p-value: 0.000367

#### Coeficiente de correlaci�n ####

# Ejemplo 38

rm(list = ls())
#Datos por pares (x,y)
x <- c(1, 1, 2, 3, 4)
y <- c(2, 3, 5, 5, 9)
# El s�mbolo ~ indica que se estudia una relaci�n entre
# la variable dependiente a la izquierda y las independientes
# a la derecha.
# La regresi�n se estudia por medio de
# los modelos lineales (linear models).
# mod es el nombre del modelo a estudiar.
mod <- lm(y ~ x)
# Liste los coeficientes de regresi�n
mod$coefficients
summary(mod)
# Halle el coeficiente de correlaci�n
cor(x,y)
# Decida la Ho: no hay correlaci�n lineal
# contra la alterna: hay correlaci�n lineal
# causada por un efecto sistem�tico.

cor.test(x, y)

# Haga el dibujo de dispersi�n,
# de las parejas de puntos (x,y)
plot(x, y)
#A�ada la l�nea de regresi�n de m�nimos cuadrados.
abline(mod)

mod$coefficients
summary(mod)

# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5 
# -0.4706  0.5294  0.5882 -1.3529  0.7059 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   0.5294     0.9825   0.539   0.6274  
# x             1.9412     0.3946   4.919   0.0161 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.029 on 3 degrees of freedom
# Multiple R-squared:  0.8897,	Adjusted R-squared:  0.8529 
# F-statistic:  24.2 on 1 and 3 DF,  p-value: 0.01609

# Halle el coeficiente de correlaci�n
cor(x,y)
# [1] 0.9432422

cor.test(x, y)


# Pearson's product-moment correlation
# 
# data:  x and y
# t = 4.9193, df = 3, p-value = 0.01609
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.3633641 0.9963528
# sample estimates:
#       cor 
# 0.9432422 

# Ejemplo 39

rm(list = ls())
#Datos por pares (x,y)
x <- c(1,2,3,4,5,6,7)
y <- c(2,5,8,9,8,5,2)
# El simbolo ~ indica que se estudia una relacion entre
# la variable dependiente a la izquierda y las independientes
# a la derecha.
# La regresion se estudia por medio de
# los modelos lineales (linear models).

# mod es el nombre del modelo a estudiar.
mod<-lm(y ~ x)

#Liste los coeficientes de regresi� on
mod$coefficients

# (Intercept)            x 
# 5.571429e+00 4.510967e-16 

summary(mod)
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5       6       7 
# -3.5714 -0.5714  2.4286  3.4286  2.4286 -0.5714 -3.5714 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) 5.571e+00  2.665e+00   2.091   0.0908 .
# x           4.511e-16  5.959e-01   0.000   1.0000  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.153 on 5 degrees of freedom
# Multiple R-squared:  1.111e-31,	Adjusted R-squared:   -0.2 
# F-statistic: 5.554e-31 on 1 and 5 DF,  p-value: 1

# Halle el coeficiente de correlacion
cor(x,y)
# [1] 0

# Decida la Ho: no hay correlacion lineal
# contra la alterna: hay correlacion lineal
# causada por un efecto sistematico.
cor.test(x, y)

# Pearson's product-moment correlation
# 
# data:  x and y
# t = 0, df = 5, p-value = 1
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.7530581  0.7530581
# sample estimates:
# cor 
#   0 
  
# Haga el dibujo de dispersion,
# de las parejas de puntos (x,y)
plot(x, y)

# A�ada la linea de regresi� on de minimos cuadrados.
abline(mod)