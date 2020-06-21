#######################################################
# UNIVERSIDAD NACIONAL DE INGENIERÍA
# CENTRO DE TECNOLOGÍAS DE INFORMACIÓN Y COMUNICACIONES
# Elaborado por: Victor Lazo
#########################################################

getwd()
setwd(dir = "c:/Users/Victor Lazo/Desktop/R4DS/05 Class/Tareas/")
rm(list=ls())

#### Construcción de Modelos y Métodos de Regresión ####

#### Regresión lineal simple ####

# Ejemplo 01
# edad: edad de una mujer
# presion: presión sanguinea
edad <- c(56, 42, 72, 36, 63, 47, 55, 47, 38, 42) 
presion <- c(148, 126, 159, 118, 149, 130, 151, 142, 114, 141)
edad
presion

# Sea la variable dependiente edad y la variable independiente presión
plot(presion, edad)

# Ajuste del modelo lineal
reg_lin <- lm(edad ~ presion)
reg_lin

# donde obtenemos el modelo lineal: 
# \beta_{0} = -43.5440, \beta_{1} = 0.6774  
# edad: \beta_{0} + \beta_{1}*presion  

# Datos de la regresión
summary(reg_lin)

# dibujamos la recta de regresión lineal
x11()
plot(presion ~ edad)
abline(reg_lin)

# Agregamos algunas caracteristicas al codigo
summary(reg_lin)
xmin <- 0.9*min(presion)
xmax <- 1.1*max(presion)
ymin <- 0.9*min(edad)
ymax <- 1.1*max(edad)
plot(presion, edad, main="Edad ~ Presion Sanguinea", sub="Pob.: 10 mujeres",
     xlab = "Presion", ylab = "Edad", 
     xlim = c(xmin, xmax), ylim = c(ymin,ymax))
abline(reg_lin)
dev.off()
plot(reg_lin)

# test de normalidad de Kolmogorov-Smirnov
ks.test(reg_lin$residuals, "pnorm")

# test de Durbin-Watson
library(lmtest)
dwtest(edad ~ presion)

# Durbin-Watson test

# data:  edad ~ presion
# DW = 1.9667, p-value = 0.5879
# alternative hypothesis: true autocorrelation is greater than 0
# En este caso, con un p-valor = 0.5879 no podemos rechazar la
# hipótesis de que los residuos son independientes.

#### Regresión lineal múltiple ####

# Ejemplo 03
# Variables:
# gastos: gasto de alimentación mensual de una familia
# ingresos: ingreso mensual.
# tamaño: tamaño de la familia
# hijosU: número de hijos en la universidad

gastos <- c(1000, 580, 520, 500, 600, 550, 400) 
ingresos <- c(50000, 2500, 2000, 1900, 3000, 4000, 2000)
tamaño <- c(7, 4, 3, 3, 6, 5, 2)
hijosU <- c(3, 1, 1, 0, 1, 2, 0)

# Agrupamos en un data frame la información relativa a las 4 variables
datos2 <- data.frame(gastos, ingresos, tamaño, hijosU)

# Ajustamos el modelo de regresión lineal múltiple
reg_lin_mul <- lm(gastos ~ ingresos + tamaño + hijosU)
summary(reg_lin_mul)

# Los coeficientes obtenidos son:
# \beta_{0}: 3.590e+02
# \beta_{1}: 7.247e-03
# \beta_{2}: 3.734e+01
# \beta_{3}: 5.359e+00
# gastos_{i} = (3.590e+02) + (7.247e-03)*ingresos_{i} + (3.734e+01)*tamano_{i} + (5.359e+00)*hijosU_{i}

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) 3.590e+02  6.291e+01   5.706   0.0107 *
#   ingresos    7.247e-03  1.802e-03   4.021   0.0276 *
#   tamaño      3.734e+01  2.046e+01   1.825   0.1655  
# hijosU      5.359e+00  4.061e+01   0.132   0.9034  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 48.57 on 3 degrees of freedom
# Multiple R-squared:  0.9677,	Adjusted R-squared:  0.9353 
# F-statistic: 29.93 on 3 and 3 DF,  p-value: 0.009772

# Ejemplo 04
library(car)
head(Prestige,5)
newdata = Prestige[,c(1:2)]
summary(newdata)
modelo = lm (income ~ education, data = newdata)
plot(newdata$education, newdata$income, main = "Educación ~ Income")
abline(modelo)
summary(modelo)

# Call:
#   lm(formula = income ~ education, data = newdata)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5493.2 -2433.8   -41.9  1491.5 17713.1 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2853.6     1407.0  -2.028   0.0452 *  
#   education      898.8      127.0   7.075 2.08e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3483 on 100 degrees of freedom
# Multiple R-squared:  0.3336,	Adjusted R-squared:  0.3269 
# F-statistic: 50.06 on 1 and 100 DF,  p-value: 2.079e-10

# \beta_{0} =-2853.6
# \beta_{1} = 898.8
# income = -2853.6 + 898.8*education

#### Modelo de Regresión logística ####

datos <- read.csv("HumanResourcesAnalytics.csv", T)
muestra <- dim(datos)[1]
datos <- datos[sample(muestra,100,replace=TRUE),]
class(datos)
str(datos)
colnames(datos) = c("nivel_satisfaccion", "ultima_evaluacion",
                    "numero_proyectos", "promedio_horas_mensuales",
                    "antiguedad", "accidente", "abandona", 
                    "promocionado", "departamento", "salario")
datos.modelo <- subset(datos, select = c(abandona, nivel_satisfaccion,
                                         ultima_evaluacion))
datos.modelo$abandona <- factor(datos.modelo$abandona)
head(datos.modelo)
plot(datos.modelo$nivel_satisfaccion, datos.modelo$abandona)

# verificamos que la variable abandona tome 2 valores
table(datos.modelo$abandona)
# Visualizamos la variable abandona
library(ggplot2)
ggplot(data = datos.modelo) + 
  geom_point(mapping = aes(x = nivel_satisfaccion,
                           y = ultima_evaluacion, color = abandona))

# resumen de cada categoria
summary(datos.modelo$nivel_satisfaccion)
summary(datos.modelo$ultima_evaluacion)
plot(datos.modelo$abandona)

modelo.logit <- glm(abandona ~ ultima_evaluacion + nivel_satisfaccion,
                    data = datos.modelo, family = "binomial")
summary(modelo.logit)

# Call:
#   glm(formula = abandona ~ ultima_evaluacion + nivel_satisfaccion, 
#       family = "binomial", data = datos.modelo)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.3859  -0.5749  -0.3330  -0.2133   2.6664  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           2.055      1.279   1.607 0.108096    
# ultima_evaluacion    -1.385      1.667  -0.831 0.406069    
# nivel_satisfaccion   -4.996      1.330  -3.757 0.000172 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 94.279  on 99  degrees of freedom
# Residual deviance: 74.038  on 97  degrees of freedom
# AIC: 80.038
# 
# Number of Fisher Scoring iterations: 5

exp(coefficients(modelo.logit))
# (Intercept)  ultima_evaluacion nivel_satisfaccion 
# 7.807856314        0.250387823        0.006767275 

# Estimar la probabilidad p de que un individuo abandone la empresa
log.odds <- predict(modelo.logit, 
                    data.frame(nivel_satisfaccion = 0.6, ultima_evaluacion = 0.75))
log.odds
# 1 
# -1.980822 

# la probabilidad de abandonar la empresa sería:
exp(log.odds)/(1-exp(log.odds))
# 1 
# 0.1600333 

#### Modelo de Regresión polimonial ####
q <- seq(from = 0,to = 20, by = 0.1)
y <- 500 + 0.4*(q - 10)^3
noise <- rnorm(length(q), mean = 10, sd = 80)
noisy.y <- y + noise
plot(q, noisy.y, col = "deepskyblue4", xlab = "q", main= "Observed data")
lines(q, y, col = "firebrick1", lwd = 3)
model <- lm(noisy.y ~ poly(q, 3))

# Intervalos de confianza de los paramentros de nuestro modelo

confint(model, level = 0.95)
#   (Intercept)  503.4007  525.2449
# poly(q, 3)1 1791.3963 2101.0913
# poly(q, 3)2 -163.9233  145.7716
# poly(q, 3)3  625.2384  934.9333

#### Regresión de Poisson ####

rm(list = ls())
NumResueltos <- c(0, 1, 2, 1, 5, 3, 2, 5, 7, 8, 12, 13, 12, 11, 10, 12, 10, 15)
HorasClase <- c(1, 3, 4, 1, 3, 5, 1, 3, 5, 2, 3, 5, 0, 3, 5, 4, 3, 5)
Nota <- c(0, 2, 3, 0, 4, 3, 1, 3, 4, 3, 4, 5, 4, 5, 4, 5, 5, 5)
tabla <- data.frame(NumResueltos, HorasClase, Nota)

regPoisson <- glm (Nota ~ NumResueltos + HorasClase, data = tabla,
                   family = poisson())
summary(regPoisson)
predict(regPoisson, type = "response")

# 1        2        3        4        5        6        7        8        9 
# 1.358463 1.820605 2.193048 1.470769 2.501516 2.641682 1.592361 2.501516 3.629678 
# 10       11       12       13       14       15       16       17       18 
# 2.853371 4.361956 5.845872 3.167194 4.028881 4.606369 4.853074 3.721239 6.852406 