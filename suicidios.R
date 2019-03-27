rm(list = ls())

library(readr)
library(dplyr)
library(readxl)
library(lubridate)
library(tidyverse)
library(ggplot2)

pibper <- read_excel("C:/Users/acamp/Desktop/Semillero Datos/pibper.xlsx")

suicidios_total <- read_excel("C:/Users/acamp/Desktop/Semillero Datos/suicidios total.xlsx")


PIBsuici <- cbind.data.frame(pibper$pibper, suicidios_total$total_suici)

names(PIBsuici) <- c("pibper", "suici_to")

tiempo      <- seq(as.Date("1985/01/12"), as.Date("2015/01/12"), by = "1 year")

suicidios_total$año <- tiempo[1:dim(suicidios_total)[1]]
suicidios <- suicidios_total %>% select(año, total_suici)

# Gráfico suicidios

suicidios %>% ggplot(aes(x = año, y = total_suici)) + geom_line(size = 1, color = "blue3") +
  labs(title = "Total suicidios en Colombia",
       x = "Tiempo",
       y = "Total suicidios", 
       caption = "Fuente: World Health Organization")
#--------------------------------------------------------------------------------------------

pibper$Año <- tiempo[1:dim(pibper)[1]]
pibper <- pibper %>% select(Año, pibper)

# Gráfico PIB per cápita

pibper %>% ggplot(aes(x = Año, y = pibper)) + geom_line(size = 1, color = "aquamarine3") +
  labs(title = "PIB per cápita",
       x = "Tiempo",
       y = "PIB per cápita (US$ a precios actuales)", 
       caption = "Fuente: World Bank")

varpibper <- read_excel("C:/Users/acamp/Desktop/Semillero Datos/varpibper.xlsx")

colombia <- read_excel("C:/Users/acamp/Desktop/Semillero Datos/colombia.xlsx")

# create data frame con variaciones del PIB

varpib_suicides <- cbind.data.frame(varpibper$PIB, suicidios_total)

names(varpib_suicides) <- c("varpib", "total_suici", "año", "poblacion", "suicitasa")

#Estadísticas descriptivas

summary(varpib_suicides)

colombia2015 <- colombia[361:372,]

mujeres <- subset(colombia2015, sexo == "female")

pie(mujeres$suici_no, mujeres$edad)

hombres <- subset(colombia2015, sexo == "male")

pie(hombres$suici_no, hombres$edad)

#________________________________________

ggplot(mujeres, aes(x=edad, y=suici_no)) + geom_bar(stat = "identity") +
  labs(x = "Rango de edad", y = "Número de suicidios")

ggplot(hombres, aes(x=edad, y=suici_no)) + geom_bar(stat = "identity") +
  labs(x = "Rango de edad", y = "Número de suicidios")
#------------------------------------------------------------------------------
ggplot(mujeres, aes(x=edad, y=suici_no)) +
  geom_point() + labs(x = "Rango de edad", y = "Número de suicidios")

ggplot(mujeres, aes(x=edad, y=suici_no, size= suicirate, color = edad)) +
  geom_point(alpha=0.9) + labs(x = "Rango de edad", y = "Número de suicidios") 
  
ggplot(hombres, aes(x=edad, y=suici_no, size= suicirate, color = edad)) +
  geom_point(alpha=0.9) + labs(x = "Rango de edad", y = "Número de suicidios") 
#------------------------------------------------------------------------------

library(latticeExtra)


xyplot(varpib_suicides$varpib + varpib_suicides$suicitasa ~ varpib_suicides$año, type = "l")

## 2=== But it could be nice to have TWO Y axis!

# --> construct separate plots for each series
obj1 <- xyplot(varpib_suicides$varpib ~ varpib_suicides$año, varpib_suicides, type = "l" , lwd=2)
obj2 <- xyplot(varpib_suicides$suicitasa ~ varpib_suicides$año, varpib_suicides, type = "l" , lwd=2)
# --> Make the plot with second y axis:
doubleYScale(obj1, obj2, add.ylab2 = TRUE)

## 3=== Same graph with a key legend
doubleYScale(obj1, obj2, text = c("obj1", "obj2") , add.ylab2 = TRUE)


ggplot(mujeres, aes(x=generacion, y=suici_no)) + geom_bar(stat = "identity") +
  labs(x = "Tipo de Generación", y = "Número de suicidios")

ggplot(hombres, aes(x=generacion, y=suici_no)) + geom_bar(stat = "identity") +
  labs(x = "Tipo de Generación", y = "Número de suicidios")

Millenials <- subset(colombia, generacion == "Millenials")

ggplot(Millenials, aes(x=edad, y=generacion)) + geom_bar(stat = "identity") +
  labs(x = "", y = "Edad")

colombia2000 <- colombia[109:372,]

colombia2000$Millenials <- ifelse(colombia2000$generacion == "Millenials", 1, 0)

colombia2000$GenerationX <- ifelse(colombia2000$generacion == "Generation X", 1, 0)

gg <-lm(suici_no ~ Millenials + GenerationX + PIBper, data = colombia2000)
summary(gg)

library(psych)
pairs.panels(colombia2000[c(4,9,11,12)])


plot(varpib_suicides$suicitasa, type = "l", main = "Suicidos por cada 100.000 habitantes", xlab = "Tiempo", ylab = "Tasa de suicidios" )
