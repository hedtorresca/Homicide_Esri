
#Análisis exploratorio de la variable homicidios

# Librerías utilizadas

library(tidyverse) # Paquete para manipulación y consulta.
library(lubridate)
library(highcharter)
library(readxl) #Paquete para lectura de datos.
library(ggplot2)
library(htmlwidgets)
library(openxlsx)
source("functions.R", encoding = 'UTF-8')

tipovar <- c("text", "date", "text", "text" , "text", "text", "text", 
             "text", "text", "text", "text", "text", "numeric",	"text",	"text",
             "text", "text", "text", "text", "numeric", "numeric","numeric",
             "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
             "numeric","numeric", "numeric","numeric","numeric","numeric","numeric","numeric",
             "numeric","numeric", "numeric") # Especificar tipo de variables del Dataset


tipovar2 <- c("numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric" , "numeric", "numeric", "numeric", "numeric" , "numeric")
#colores de las series

col <-   c( "#8cc63f", # verde
            "#f15a24", # naranja
            "#0071bc", # azul vivo
            "#6d6666", # gris
            "#fbb03b", # amarillo
            "#93278f", # morado
            "#29abe2", # azul claro
            "#c1272d", # rojo
            "#8b7355", # cafe
            "#855b5b", # vinotinto
            "#ed1e79") # rosado

#Lectura de bases de datos

# homicide <- read_excel("datasetanual.xlsx", 
#                            sheet = 1,  col_types = tipovar)

historico <- read_excel("HISTORICO.xls", 
                       sheet = 1)

Poblacion <- read_excel("ProyeccionDane2.xlsx", col_types = tipovar2)



### Organizar información

##### Para Histórico Policía

for(i in historico$`MUNICIPIO DEL HECHO`){
  historico$CODMUN[historico$`MUNICIPIO DEL HECHO`==i] <- Poblacion$DPMP[Poblacion$MUNICIPIO == i]
}


for(i in historico$CODMUN){
  historico$DEPARTAMENTO[historico$CODMUN==i] <- Poblacion$DEPARTAMENTO[Poblacion$DPMP == i]
}

##### Para tasas


propor <- tibble(0, 1122, 26)

for(i in 1:1122){
  propor[i,2] <- Poblacion$MUNICIPIO[i]
}

for(i in 1:1122){
  propor[i,3] <- Poblacion$DEPARTAMENTO[i]
}


for(i in 1:1122){
  propor[i,4] <- paste0(Poblacion$MUNICIPIO[i], "-", Poblacion$DEPARTAMENTO[i])
}

for(i in 1:1122){
  propor[i,1] <- Poblacion$DPMP[i]
}


for(k in 5:20){
for(i in historico$CODMUN){
  propor[propor[,1]==i,k] <- (historico[historico$CODMUN == i, k-3]/Poblacion[Poblacion$DPMP == i,k+2])*100000
}
}

##### Para conteos desde 2003


propor2 <- tibble(0, 1122, 46)

for(i in 1:1122){
  propor2[i,2] <- Poblacion$MUNICIPIO[i]
}

for(i in 1:1122){
  propor2[i,3] <- Poblacion$DEPARTAMENTO[i]
}


for(i in 1:1122){
  propor2[i,4] <- paste0(Poblacion$MUNICIPIO[i], " - ", Poblacion$DEPARTAMENTO[i])
}

for(i in 1:1122){
  propor2[i,1] <- Poblacion$DPMP[i]
}



for(k in 5:20){
  for(i in historico$CODMUN){
    propor2[propor2[,1]==i,k] <- historico[historico$CODMUN == i, k-3]
  }
}

for(k in 21:36){
  for(i in historico$CODMUN){
    propor2[propor2[,1]==i,k] <- Poblacion[Poblacion$DPMP == i,k+2-16]
  }
}


TotPoblacion <-  rowSums (select (propor2, contains (".1")))
TotHomicide <- rowSums(propor2[,5:20])
MeanTasa <- rowMeans(propor[,5:20])
SdTasa <- apply(propor[,5:20],1,sd) 
CVTasa<- 100*SdTasa/MeanTasa
propor[,21] <-MeanTasa 
propor[,22] <-SdTasa
propor[,23] <-CVTasa



for(k in 37:52){
  for(i in historico$CODMUN){
    propor2[propor2[,1]==i,k] <- ((propor2[propor2[,1]==i,k-32]*propor2[propor2[,1]==i,k-16])/propor[propor[,1]==i,21])
  }
}



for(k in 21:36){
for(i in historico$CODMUN){
  propor[propor[,1]==i,k] <- ((propor[propor[,1]==i,k-16]*propor[propor2[,1]==i,k-16])/propor[propor[,1]==i,21])
}
}


colnames(propor)[1] <- "CÓDIGO-MUNICIPIO"
colnames(propor)[2] <- "MUNICIPIO"
colnames(propor)[3] <- "DEPARTAMENTO"
colnames(propor)[4] <- "MUNICIPIO-DEPARTAMENTO"

colnames(propor2)[1] <- "CÓDIGO-MUNICIPIO"
colnames(propor2)[2] <- "MUNICIPIO"
colnames(propor2)[3] <- "DEPARTAMENTO"
colnames(propor2)[4] <- "MUNICIPIO-DEPARTAMENTO"

TotPoblacion <-  rowSums (select (propor2, contains (".1")))
TotHomicide <- rowSums(propor2[,5:20])
MeanTasa <- rowMeans(propor[,5:20])
SdTasa <- apply(propor[,5:20],1,sd) 
CVTasa<- 100*SdTasa/MeanTasa
propor[,21] <-MeanTasa 
propor[,22] <-SdTasa
propor[,23] <-CVTasa

colnames(propor)[21] <- "TASA PROMEDIO"
colnames(propor)[22] <- "DESVIACIÓN ESTÁNDAR"
colnames(propor)[23] <- "COEFICIENTE DE VARIACIÓN"

write.xlsx(propor, file = "Tasas2.xlsx", sheetName = "Tasas y conteos")


Final <- cbind(propor[,1:23], propor2[,5:20])
write.xlsx(Final, file = "Tasas.xlsx", sheetName = "Tasas y conteos")

