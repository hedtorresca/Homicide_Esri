#Análisis exploratorio de la variable homicidios

# Librerías utilizadas

library(tidyverse) # Paquete para manipulación y consulta.
library(lubridate)
library(highcharter)
library(readxl) #Paquete para lectura de datos.
library(ggplot2)
library(htmlwidgets)
source("code/functions.R", encoding = 'UTF-8')

tipovar <- c("text", "date", "text", "text" ,"numeric") # Especificar tipo de variables del Dataset


tipovar2 <- c("numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric" , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text" )
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

homicide <- read_excel("data/Homicidios_Unitarios.xlsx", col_types = tipovar)

historico <- read_excel("data/historico.xls", 
                        sheet = 1)

Poblacion <- read_excel("data/ProyeccionDane2003-N.xlsx", col_types = tipovar2)

homicide$WEEK <- as.character(week(homicide$FECHA)) #Convierte en la semana correspondiente la fecha
homicide$YEAR <- as.character(year(homicide$FECHA))#Año
homicide$MONTH <- as.character(month(homicide$FECHA))

for (i in 1:9){
  homicide$WEEK[homicide$WEEK==i]= paste0("0",i)
}
for (i in 1:9){
  homicide$MONTH[homicide$MONTH==i]= paste0("0",i)
}

homicide$`MUN-DEPT` <- gsub("-", " - ", homicide$`MUN-DEPT`  )

conteo <- function(varc){
  homicide %>% group_by_(.dots = list("YEAR",varc)) %>% 
    summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
    select(YEAR, Clase, Total)
}

cant_departamento <- conteo("DEPARTAMENTO")
cant_municipio <- conteo("MUNICIPIO")
cant_mun_dept <- conteo("`MUN-DEPT`")

Poblacion$NOMBRE <- gsub("-", " - ", Poblacion$NOMBRE)

  tipovar2 <- c("text", "text", "text", "text")
  
  repetidos <- read_excel("data_generate/repetidos.xlsx")

  i= "ALBÁN - CUNDINAMARCA"
  k= 2010
  for(i in repetidos$`MUNICIPIO-DEPARTAMENTO`){
    for(k in 2010:2018){
    todos[colnames(select(todos[todos$`Municipio - Departamento` == i,], contains(paste0(k,".conteo"))))][rownames(select(todos[todos$`Municipio - Departamento` == i,], contains(paste0(k,".conteo")))),] <- max(0,cant_mun_dept$Total[cant_mun_dept$YEAR == k & cant_mun_dept$Clase == i])
    todos[colnames(select(todos[todos$`Municipio - Departamento` == i,], contains(paste0(k,".tasa"))))][rownames(select(todos[todos$`Municipio - Departamento` == i,], contains(paste0(k,".tasa")))),] <- (max(0,cant_mun_dept$Total[cant_mun_dept$YEAR == k & cant_mun_dept$Clase == i])/select(Poblacion[Poblacion$NOMBRE == i,], contains(paste(k))))*100000
    todos[colnames(select(todos[todos$`Municipio - Departamento` == i,], contains(paste0(k,".fuente"))))][rownames(select(todos[todos$`Municipio - Departamento` == i,], contains(paste0(k,".fuente")))),] <- "Policía Reciente"
    
    }
    
   
    }


    
    
    
 