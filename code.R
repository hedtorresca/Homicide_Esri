
#Análisis exploratorio de la variable homicidios

# Librerías utilizadas

library(tidyverse) # Paquete para manipulación y consulta.
library(lubridate)
library(highcharter)
library(readxl) #Paquete para lectura de datos.
library(ggplot2)
library(htmlwidgets)
source("functions.R", encoding = 'UTF-8')

tipovar <- c("text", "date", "text", "text" , "text", "text", "text", 
             "text", "text", "text", "text", "text", "numeric",	"text",	"text",
             "text", "text", "text", "text", "numeric", "numeric","numeric",
             "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
             "numeric","numeric", "numeric","numeric","numeric","numeric","numeric","numeric",
             "numeric","numeric", "numeric") # Especificar tipo de variables del Dataset

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


homicide <- read_excel("datasetanual.xlsx", 
                           sheet = 1,  col_types = tipovar)

homicide$WEEK <- as.character(week(homicide$FECHA)) #Convierte en la semana correspondiente la fecha
homicide$YEAR <- as.character(year(homicide$FECHA))#Año

for (i in 1:9){
homicide$WEEK[homicide$WEEK==i]= paste0("0",i)
}

Clases <- function(varc){
  homicide %>% group_by_(.dots = list("WEEK",varc)) %>% 
    summarise(Total = n()) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, WEEK, Clase, Total)
}

DT1 <- Clases("YEAR")
DT1
CAT_ANO_SERIE1 <- series(
  datos = DT1,
  categoria = "YEAR",
  colores = col,
  titulo = "Evolución por semanas del número de homicidios en cada año",
  eje = "Número de homicidios"
); CAT_ANO_SERIE1

saveWidget(CAT_ANO_SERIE1, file = file.path(getwd() ,  "Seriesemanal.html")  ,  selfcontained = F , libdir = "libraryjs")

