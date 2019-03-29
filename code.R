
#Análisis exploratorio de la variable homicidios

# Librerías utilizadas

library(tidyverse) # Paquete para manipulación y consulta.
library(lubridate)
library(highcharter)
library(readxl) #Paquete para lectura de datos.
library(ggplot2)
library(htmlwidgets)
library(xlsx)
source("functions.R", encoding = 'UTF-8')

tipovar <- c("text", "date", "text", "text" , "text", "text", "text", 
             "text", "text", "text", "text", "text", "numeric",	"text",	"text",
             "text", "text", "text", "text", "numeric", "numeric","numeric",
             "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
             "numeric","numeric", "numeric","numeric","numeric","numeric","numeric","numeric",
             "numeric","numeric", "numeric") # Especificar tipo de variables del Dataset


tipovar2 <- c("numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric" , "numeric", "numeric", "numeric", "numeric", "numeric" )
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

for(i in 1:1112){
  propor[i,2] <- Poblacion$MUNICIPIO[i]
}

for(i in 1:1112){
  propor[i,3] <- Poblacion$DEPARTAMENTO[i]
}


for(i in 1:1112){
  propor[i,4] <- paste0(Poblacion$MUNICIPIO[i], "-", Poblacion$DEPARTAMENTO[i])
}

for(i in 1:1112){
  propor[i,1] <- Poblacion$DPMP[i]
}



for(k in 5:20){
for(i in historico$CODMUN){
  propor[propor[,1]==i,k] <- (historico[historico$CODMUN == i, k-3]/Poblacion[Poblacion$DPMP == i,k+2])*100000
}
}

##### Para conteos desde 2003


propor2 <- tibble(0, 1122, 46)

for(i in 1:1112){
  propor2[i,2] <- Poblacion$MUNICIPIO[i]
}

for(i in 1:1112){
  propor2[i,3] <- Poblacion$DEPARTAMENTO[i]
}


for(i in 1:1112){
  propor2[i,4] <- paste0(Poblacion$MUNICIPIO[i], "-", Poblacion$DEPARTAMENTO[i])
}

for(i in 1:1112){
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

propor[,21] <- TotPoblacion



for(k in 37:52){
for(i in historico$CODMUN){
  propor2[propor2[,1]==i,k] <- ((propor2[propor2[,1]==i,k-32]*propor2[propor2[,1]==i,k-16])/propor[propor[,1]==i,21])
}
}

Promedio <-  rowMeans (select (propor2, contains (".1")))

Conteo <- rowSums (select (propor2, contains (".2")))
 
Tasa <- (Conteo/Promedio)*100000

Conteo[1]

Final[,37] <- Tasa

colnames(propor)[1] <- "CÓDIGO-MUNICIPIO"
colnames(propor)[2] <- "MUNICIPIO"
colnames(propor)[3] <- "DEPARTAMENTO"
colnames(propor)[4] <- "MUNICIPIO-DEPARTAMENTO"

colnames(propor2)[1] <- "CÓDIGO-MUNICIPIO"
colnames(propor2)[2] <- "MUNICIPIO"
colnames(propor2)[3] <- "DEPARTAMENTO"
colnames(propor2)[4] <- "MUNICIPIO-DEPARTAMENTO"

propor[21] <- Tasa

colnames(Final)[37] <- "Tasa Promedio"

Final <- full_join(propor, propor2, by=c("CÓDIGO-MUNICIPIO", "MUNICIPIO", "DEPARTAMENTO","MUNICIPIO-DEPARTAMENTO"))


write.xlsx(Final, file = "Tasas.xlsx", sheetName = "Tasas y conteos")

na.omit(propor2)
for(i in 1:)

#### Clasificación de ciudades

grandes[,2] <- Poblacion$MUNICIPIO

for(i in Poblacion$`2018`){
  grandes <- Poblacion$DPMP[Poblacion$`2018` > 1000000]
}

for(i in Poblacion$`2018`){
  intermedias <- Poblacion$DPMP[Poblacion$`2018` < 1000000 & Poblacion$`2018` > 100000]
}

for(i in Poblacion$`2018`){
  peques <- Poblacion$DPMP[Poblacion$`2018` < 100000 & Poblacion$`2018` > 20000]
}

for(i in Poblacion$`2018`){
  muypeques <- Poblacion$DPMP[Poblacion$`2018` < 20000]
}


for(i in Poblacion$`2018`){
  grandes <- Poblacion$DPMP[Poblacion$`2018` > 1000000]
}
#### Gráficos según BD POLICIA 2010

homicide$WEEK <- as.character(week(homicide$FECHA)) #Convierte en la semana correspondiente la fecha
homicide$YEAR <- as.character(year(homicide$FECHA)) #Año
homicide$MONTH <- as.character(month(homicide$FECHA)) #meses

for (i in 1:9){
homicide$WEEK[homicide$WEEK==i]= paste0("0",i)
}

for (i in 1:9){
  homicide$MONTH[homicide$MONTH==i]= paste0("0",i)
}

## Funciones conteo

Conteo <- function(varc){
  homicide %>% group_by_(.dots = list("WEEK",varc)) %>% 
    summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, WEEK, Clase, Total)
}

Conteo2 <- function(varc){
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
); serie_global

saveWidget(serie_global, file = file.path(getwd() ,  "Seriesemanal.html")  ,  selfcontained = F , libdir = "libraryjs")


#########################
###Serie por municipios
########################

escogidos <- c("RESTREPO-META")


####  Semanas 

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
    
    saveWidget(serie_global, file = file.path(getwd(), "municipios", paste0("serie_semanal", d, ".html"))  ,  selfcontained = F , libdir = "libraryjs")
  
}

#### Meses

for(d in escogidos){
  filtro <- homicide[homicide$`MUN-DEPT`==d,] 
  Conteo <- function(varc){
    filtro %>% group_by_(.dots = list("MONTH",varc)) %>% 
      summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
      mutate_(Variable = "varc") %>% select(Variable, MONTH, Clase, Total)
  }
  
  meses <- Conteo("YEAR")
  
  
  serie_global <- series3(
    datos = meses,
    categoria = "YEAR",
    colores = col,
    titulo = paste0(d,": Evolución por meses del número de homicidios en cada año"),
    eje = "Número de homicidios"
  ) 
  
  saveWidget(serie_global, file = file.path(getwd(), "municipios", paste0("serie_mes_", d, ".html"))  ,  selfcontained = F , libdir = "libraryjs")
  
}

#### Años 

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



#### Por años
Conteo3 <- function(varc){
  homicide %>% group_by_(.dots = list("YEAR",varc)) %>% 
    summarise(Total = sum(HOMICIDIOS)) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, YEAR, Clase, Total)
}

years <- Conteo3("SEXO")


serie_global <- series2(
  datos = years,
  categoria = "SEXO",
  colores = col,
  titulo =  "Evolución por semanas del número de homicidios según sexo biológico",
  eje = "Número de homicidios"
); serie_global

saveWidget(serie_global, file = file.path(getwd() ,  "desagregaciones", "sexo (años).html")  ,  selfcontained = F , libdir = "libraryjs")


#### Según sea zona rural o urbana



years2 <- Conteo3("ZONA")


serie_global <- series2(
  datos = years2,
  categoria = "ZONA",
  colores = col,
  titulo =  "Evolución por semanas del número de homicidios según zona",
  eje = "Número de homicidios"
); serie_global

saveWidget(serie_global, file = file.path(getwd() ,  "desagregaciones", "zona.html")  ,  selfcontained = F , libdir = "libraryjs")

#### Según sea el sitio específico

homicide$CLASE_SITIO[homicide$CLASE_SITIO != "RIOS" & homicide$CLASE_SITIO != "FRENTE A RESIDENCIAS   VIA PUBLICA" & homicide$CLASE_SITIO != "VIAS PUBLICAS" & homicide$CLASE_SITIO != "FINCAS Y SIMILARES" & homicide$CLASE_SITIO != "CASAS DE HABITACION" & homicide$CLASE_SITIO != "DENTRO DE LA VIVIENDA" & homicide$CLASE_SITIO != "BARES, CANTINAS Y SIMILARES" & homicide$CLASE_SITIO != "CARRETERAS"] = "OTRO LUGAR"



years3 <- Conteo3("CLASE_SITIO")


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

years3 <- Conteo3("ARMA_EMPLEADA")



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


years3 <- Conteo3("`PAIS NACE`")


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







