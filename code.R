
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

homicide <- read_excel("datasetanual.xlsx", 
                           sheet = 1,  col_types = tipovar)

homicide$WEEK <- as.character(week(homicide$FECHA)) #Convierte en la semana correspondiente la fecha
homicide$YEAR <- as.character(year(homicide$FECHA))#Año


for (i in 1:9){
homicide$WEEK[homicide$WEEK==i]= paste0("0",i)
}


Conteo <- function(varc){
  homicide %>% group_by_(.dots = list("WEEK",varc)) %>% 
    summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, WEEK, Clase, Total)
}


semanas <- Conteo("YEAR")

# Serie Nacional

serie_global <- series(
  datos = semanas,
  categoria = "YEAR",
  colores = col,
  titulo = "Evolución por semanas del número de homicidios en cada año",
  eje = "Número de homicidios"
); serei_global

saveWidget(serie_global, file = file.path(getwd() ,  "Seriesemanal.html")  ,  selfcontained = F , libdir = "libraryjs")


#########################
###Serie por municipios###}
########################

for(d in homicide$`MUN-DEPT`){
  filtro <- homicide[homicide$`MUN-DEPT`==d,] 
  Conteo <- function(varc){
    filtro %>% group_by_(.dots = list("WEEK",varc)) %>% 
      summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
      mutate_(Variable = "varc") %>% select(Variable, WEEK, Clase, Total)
  }
    
    semanas <- Conteo("YEAR")
    
    
    serie_global <- series(
      datos = semanas,
      categoria = "YEAR",
      colores = col,
      titulo = paste0(d,": Evolución por semanas del número de homicidios en cada año"),
      eje = "Número de homicidios"
    ); serie_global
    
    saveWidget(serie_global, file = file.path(getwd(),  paste0("serie_", d, ".html"))  ,  selfcontained = F , libdir = "libraryjs")
  
}

#####################
## Series departamentos##
####################

for(d in homicide$DEPARTAMENTO){
  filtro <- homicide[homicide$DEPARTAMENTO==d,] 
  Conteo <- function(varc){
    filtro %>% group_by_(.dots = list("WEEK",varc)) %>% 
      summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
      mutate_(Variable = "varc") %>% select(Variable, WEEK, Clase, Total)
  }
  
  semanas <- Conteo("YEAR")
 
   for(i in levels("YEAR"))
  for(i in 1:53){
    weeks
  }
    
  
  
  serie_global <- series(
    datos = semanas,
    categoria = "YEAR",
    colores = col,
    titulo = paste0(d,": Evolución por semanas del número de homicidios en cada año"),
    eje = "Número de homicidios"
  ); serie_global
  
  saveWidget(serie_global, file = file.path(getwd(),  "series_dept", paste0("serie_", d, ".html"))  ,  selfcontained = F , libdir = "libraryjs")
  
}

######################
### Series por género
#####################


#### Por semanas
Conteo <- function(varc){
  homicide %>% group_by_(.dots = list("WEEK",varc)) %>% 
    summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, WEEK, Clase, Total)
}


semanas <- Conteo("SEXO")

serie_global <- series(
  datos = semanas,
  categoria = "SEXO",
  colores = col,
  titulo =  "Evolución por semanas del número de homicidios según sexo biológico",
  eje = "Número de homicidios"
); serie_global



#### Por años
Conteo2 <- function(varc){
  homicide %>% group_by_(.dots = list("YEAR",varc)) %>% 
    summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, YEAR, Clase, Total)
}

years <- Conteo2("SEXO")


serie_global <- series2(
  datos = years,
  categoria = "SEXO",
  colores = col,
  titulo =  "Evolución por semanas del número de homicidios según sexo biológico",
  eje = "Número de homicidios"
); serie_global



#### Según sea zona rural o urbana



years2 <- Conteo2("ZONA")


serie_global <- series2(
  datos = years2,
  categoria = "ZONA",
  colores = col,
  titulo =  "Evolución por semanas del número de homicidios según zona",
  eje = "Número de homicidios"
); serie_global

#### Según sea el sitio espeecífico

homicide$CLASE_SITIO[homicide$CLASE_SITIO != "RIOS" & homicide$CLASE_SITIO != "FRENTE A RESIDENCIAS   VIA PUBLICA" & homicide$CLASE_SITIO != "VIAS PUBLICAS" & homicide$CLASE_SITIO != "FINCAS Y SIMILARES" & homicide$CLASE_SITIO != "CASAS DE HABITACION" & homicide$CLASE_SITIO != "DENTRO DE LA VIVIENDA" & homicide$CLASE_SITIO != "BARES, CANTINAS Y SIMILARES" & homicide$CLASE_SITIO != "CARRETERAS"] = "OTRO LUGAR"



years3 <- Conteo2("CLASE_SITIO")


table(homicide$CLASE_SITIO)

serie_global <- series2(
  datos = years3,
  categoria = "CLASE_SITIO",
  colores = col,
  titulo =  "Evolución por semanas del número de homicidios según zona",
  eje = "Número de homicidios"
); serie_global

#### Según sea el tipo de arma

homicide$ARMA_EMPLEADA[homicide$ARMA_EMPLEADA != "ARMA BLANCA / CORTOPUNZANTE" & homicide$ARMA_EMPLEADA != "ARMA DE FUEGO" & homicide$ARMA_EMPLEADA != "CONTUNDENTES" & homicide$ARMA_EMPLEADA != "MINA ANTIPERSONA" ] = "OTRA"

years3 <- Conteo2("ARMA_EMPLEADA")



serie_global <- series2(
  datos = years3,
  categoria = "ARMA_EMPLEADA",
  colores = col,
  titulo =  "Evolución por semanas del número de homicidios según zona",
  eje = "Número de homicidios"
); serie_global


##### Nacionalidad

homicide$`PAIS NACE`[homicide$`PAIS NACE` != "COLOMBIA" & homicide$`PAIS NACE` != "DESCONOCIDO"] = "EXTRANJERO"


years3 <- Conteo2("`PAIS NACE`")


serie_global <- series2(
  datos = years3,
  categoria = "`PAIS NACE`",
  colores = col,
  titulo =  "Evolución por semanas del número de homicidios según nacionalidad",
  eje = "Número de homicidios"
); serie_global

homicide$EDAD
