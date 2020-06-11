#### Introducción ####
## Este documento es un script de R
## Se puede agrupar código para su ejecución asincrónica
## La ejecución de los comando de R se realiza usando 
## Ctrl + Enter (Windows)
## Cmd + Enter (MacOS)
## El símbolo "#" sirve para comentar el código
#### Limpiar el entorno de trabajo ####
rm(list = ls())
#### Cargar los datos ####
# https://ourworldindata.org/grapher/learning-outcomes-vs-gdp-per-capita?time=latest
# REAL GDP PER CAPITA IN 2011US$
logdp <- read.csv("logdp.csv")
dim(logdp)
names(logdp)
class(logdp)
#### Manipulación de los datos ####
logdp <- logdp[c(-1)]
head(logdp)

(colnames(logdp))[5]
colnames(logdp)[3] <- "score"
colnames(logdp)[4] <- "GDPcap"
head(logdp)

#### Selección del país ####
# swe <- logdp[which(logdp$Code == "SWE"),]
swe <- logdp[logdp$Code == "SWE",]
dim(swe)
summary(swe[c(-1,-2)])
sapply(swe[c(-1,-2)], sd, na.rm = TRUE)

#### Estimación del modelo de regresión lineal ####
attach(swe)

m <- lm(GDPcap ~ score + Population)
summary(m)
plot(m)
shapiro.test(m$residuals)
library(lmtest)
bptest(m)
resettest(m, power = 2, type = "regressor")
