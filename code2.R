
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


tipovar2 <- c("numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric" , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric" )
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

historico <- read_excel("HISTORICO.xls", 
                       sheet = 1)


Poblacion <- read_excel("ProyeccionDane.xlsx", col_types = tipovar2)

historico$CODMUN <- 0

for(i in historico$`MUNICIPIO DEL HECHO`){
  historico$CODMUN[historico$`MUNICIPIO DEL HECHO`==i] <- Poblacion$DPMP[Poblacion$MUNICIPIO == i]
}


for(i in historico$CODMUN){
  historico$DEPARTAMENTO[historico$CODMUN==i] <- Poblacion$DEPARTAMENTO[Poblacion$DPMP == i]
}

View(historico)
head(historico)

propor <- tibble(0, 1122, 26)

for(i in 1:1112){
  propor[i,1] <- Poblacion$MUNICIPIO[i]
}

for(i in 1:1112){
  propor[i,2] <- Poblacion$DPMP[i]
}

for(i in 1:1112){
  propor[i,3] <- paste0(Poblacion$MUNICIPIO[i], "-", Poblacion$DEPARTAMENTO[i])
}

Poblacion$MUNDEPT <- 0
for(i in 1:1112){
  Poblacion$MUNDEPT[i] <- paste0(Poblacion$MUNICIPIO[i], "-", Poblacion$DEPARTAMENTO[i])
}

for(k in 6:19){
for(i in historico$CODMUN){
  propor[propor[,2]==i,k-2] <- (historico[historico$CODMUN == i, k-2]/Poblacion[Poblacion$DPMP == i,k+3])*100000
}
}

for(i in Poblacion$`2018`){
  grandes <- Poblacion$MUNDEPT[Poblacion$`2018` > 1000000]
}
grandes


for(i in Poblacion$`2018`){
  intermedias <- Poblacion$MUNDEPT[Poblacion$`2018` < 1000000 & Poblacion$`2018` > 100000]
}

intermedias

for(i in Poblacion$`2018`){
  peques <- Poblacion$MUNDEPT[Poblacion$`2018` < 100000 & Poblacion$`2018` > 20000]
}

for(i in Poblacion$`2018`){
  muypeques <- Poblacion$MUNDEPT[Poblacion$`2018` < 20000]
}

View(propor)

propor <- na.omit(propor)

Variable <- as.tibble(matrix(nrow= nrow(propor)*16, ncol = 1, "Municipio"))
  YEAR <- as.tibble( rep(c(2003:2018), (nrow(propor))))

repet <- as.matrix(0, nrow= 1,ncol= 16)
repet <- t(rep(propor$[1], 14))
for(i in 2:nrow(propor)-1){
repetir  <- t(rep(propor$`26`[i], 16))
repet <- cbind(repet, repetir)
}

tasa <- propor[1,4:17]
for(i in 2:nrow(propor)){
tasas <- propor[i,4:17]
tasa <- cbind(tasa, tasas)
}


repet <- t(repet)
 
tasa <- t(tasa)

View(repet) 


Clase <- as.tibble(repet)
Total <- as.tibble(tasa)

homicidios <- bind_cols(Variable, Year, Clase, Total)

homicide$WEEK <- as.character(week(homicide$FECHA)) #Convierte en la semana correspondiente la fecha
homicide$YEAR <- as.character(year(homicide$FECHA))#Año
homicide$MONTH <- as.character(month(homicide$FECHA))

for (i in 1:9){
homicide$WEEK[homicide$WEEK==i]= paste0("0",i)
}
for (i in 1:9){
  homicide$MONTH[homicide$MONTH==i]= paste0("0",i)
}


Conteo <- function(varc){
  homicide %>% group_by_(.dots = list("WEEK",varc)) %>% 
    summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, WEEK, Clase, Total)
}

Conteo3 <- function(varc){
  homicide %>% group_by_(.dots = list("MONTH",varc)) %>% 
    summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, MONTH, Clase, Total)
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

escogidos <- c("ACACÏAS-META", "TARAZÁ-ANTIOQUIA", "TIBÚ-NORTE DE SANTANDER")

for(d in escogidos){
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
    ) 
    
    saveWidget(serie_global, file = file.path(getwd(), "municipios", paste0("serie_", d, ".html"))  ,  selfcontained = F , libdir = "libraryjs")
  
}


for(d in escogidos){
  filtro <- homicide[homicide$`MUN-DEPT`==d,] 
  Conteo3 <- function(varc){
    filtro %>% group_by_(.dots = list("MONTH",varc)) %>% 
      summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
      mutate_(Variable = "varc") %>% select(Variable, MONTH, Clase, Total)
  }
  
  meses <- Conteo3("YEAR")
  
  
  serie_global <- series3(
    datos = meses,
    categoria = "YEAR",
    colores = col,
    titulo = paste0(d,": Evolución por meses del número de homicidios en cada año"),
    eje = "Número de homicidios"
  ) 
  
  saveWidget(serie_global, file = file.path(getwd(), "municipios", paste0("serie_mes_", d, ".html"))  ,  selfcontained = F , libdir = "libraryjs")
  
}


for(d in escogidos){
  filtro <- homicide[homicide$`MUN-DEPT`==d,] 
  Total <- filtro %>% group_by(YEAR) %>%  summarise(Total = sum(HOMICIDIOS)) %>% ungroup() %>% 
    mutate(Variable="TOTAL", YEAR=YEAR, Clase = "Total", Total=Total) %>% 
    select(Variable, YEAR, Clase, Total)
  
  
  
  serie_global <- series2(
    datos = Total,
    categoria = "TOTAL",
    colores = col,
    titulo =  paste0(d,": Evolución histórica anual"),
    eje = "Número de homicidios"
  ); serie_global
  
  saveWidget(serie_global, file = file.path(getwd(), "municipios", paste0("serie_anual_", d, ".html"))  ,  selfcontained = F , libdir = "libraryjs")
  
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

saveWidget(serie_global, file = file.path(getwd() ,  "desagregaciones", "sexo(semanas).html")  ,  selfcontained = F , libdir = "libraryjs")


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

saveWidget(serie_global, file = file.path(getwd() ,  "desagregaciones", "sexo (años).html")  ,  selfcontained = F , libdir = "libraryjs")


#### Según sea zona rural o urbana



years2 <- Conteo2("ZONA")


serie_global <- series2(
  datos = years2,
  categoria = "ZONA",
  colores = col,
  titulo =  "Evolución por semanas del número de homicidios según zona",
  eje = "Número de homicidios"
); serie_global

saveWidget(serie_global, file = file.path(getwd() ,  "desagregaciones", "zona.html")  ,  selfcontained = F , libdir = "libraryjs")

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

saveWidget(serie_global, file = file.path(getwd() ,  "desagregaciones", "lugar del crimen.html")  ,  selfcontained = F , libdir = "libraryjs")

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

saveWidget(serie_global, file = file.path(getwd() ,  "desagregaciones", "arma empleada.html")  ,  selfcontained = F , libdir = "libraryjs")


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

saveWidget(serie_global, file = file.path(getwd() ,  "desagregaciones", "nacionalidad.html")  ,  selfcontained = F , libdir = "libraryjs")

Total <- homicide %>% group_by(YEAR) %>%  summarise(Total = sum(HOMICIDIOS)) %>% ungroup() %>% 
  mutate(Variable="TOTAL", YEAR=YEAR, Clase = "Total", Total=Total) %>% 
  select(Variable, YEAR, Clase, Total)



serie_global <- series2(
  datos = Total,
  categoria = "TOTAL",
  colores = col,
  titulo =  "Evolución histórica general",
  eje = "Número de homicidios"
); serie_global

saveWidget(serie_global, file = file.path(getwd() ,   "serie general.html")  ,  selfcontained = F , libdir = "libraryjs")







