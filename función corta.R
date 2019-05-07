library(tidyverse) # Paquete para manipulación y consulta.
library(readxl) #Paquete para lectura de datos.
library(openxlsx)



#Lectura

tipovar2 <- c("numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric" , "numeric", "numeric", "numeric", "numeric", "numeric" )

Poblacion <- read_excel("ProyeccionDane.xlsx", col_types = tipovar2)


### Lectura de archivos

nfile <- 8 # Especificar número de archivos
for(i in 1:nfile ){
file <- paste0("Data/Municipios", i, ".xlsx")
rownames <- getSheetNames(file)
rownames <- gsub(" ", "", rownames)


setconteos <- NULL
setfuentes <- NULL


for(i in 1:length(rownames)){
  municipio <- read_excel(file, 
                          sheet = i, range = "B3:D19", col_names = c("Medicina Legal", "Policía", "Fiscalía")) 
  
  municipio <- as.data.frame(t(municipio))
  major <- matrix(ncol = 17, nrow = 1, 0)
  fuente <- matrix(ncol = 17, nrow = 1, 0)
  
  for(i in  1:17){
    major[i] <- municipio[order(municipio[,i], decreasing = TRUE)[1],i]
    fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing = TRUE)[1]]
  }
  setconteos <- rbind(setconteos, major)
  setfuentes <- rbind(setfuentes, fuente)
}

row.names(setconteos) <- rownames
colnames(setconteos) <- c("2002.conteo", "2003.conteo", "2004.conteo", "2005.conteo", "2006.conteo", "2007.conteo", "2008.conteo", "2009.conteo", "2010.conteo", "2011.conteo", "2012.conteo", "2013.conteo", "2014.conteo", "2015.conteo", "2016.conteo", "2017.conteo", "2018.conteo")

row.names(setfuentes) <- rownames
colnames(setfuentes) <- c("2002.fuente", "2003.fuente", "2004.fuente", "2005.fuente", "2006.fuente", "2007.fuente", "2008.fuente", "2009.fuente", "2010.fuente", "2011.fuente", "2012.fuente", "2013.fuente", "2014.fuente", "2015.fuente", "2016.fuente", "2017.fuente", "2018.fuente")


settasas <- matrix(0, ncol = 4, nrow = length(rownames))
for(i in 1:length(rownames)){
  settasas[i,1] <- rownames[i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,2] <- Poblacion$MUNICIPIO[Poblacion$DPMP == i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,3] <- Poblacion$DEPARTAMENTO[Poblacion$DPMP == i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,4] <- paste0(Poblacion$MUNICIPIO[Poblacion$DPMP == i], " - ", Poblacion$DEPARTAMENTO[Poblacion$DPMP == i])
}

colnames(settasas) <- c("Código", "Municipio", "Departamento", "Municipio - Departamento")


x <- as_tibble(cbind(settasas, matrix(0, ncol=17, nrow=length(rownames)), setconteos, setfuentes))

filename <- basename(file)
idfile <- substr(filename, 11,11)



for(k in 5:21){
  for(i in t(x[,1])){
    x[x[,1]==i,k] <- (as.numeric(x[x[,1]==i, k + 17])/Poblacion[Poblacion$DPMP == i,k+2])*100000
  }
  colnames(x)[k] <- paste0(2000+k-3,".tasa")
}

assign(paste0("settasas", idfile), x)
}

settasasall <- NULL
for(i in 1:nfile){
settasasall <- unique(rbind(settasasall, eval(as.name(paste0("settasas",i)))))
}


settasasall$promedio.tasa <- apply(apply(select (settasasall, contains (".tasa")),2,as.numeric),1,mean) 
settasasall$sd.tasa <- apply(apply(select (settasasall, contains (".tasa")),2,as.numeric),1,sd)
settasasall$cv.tasa <- 100*settasasall$sd.tasa/settasasall$promedio.tasa
                        
settasasalldef <- cbind(settasasall[,1:21], settasasall$promedio.tasa, settasasall$sd.tasa, settasasall$cv.tasa, settasasall[,22:55])
colnames(settasasalldef)[22:24] <- c("TASA PROMEDIO", "DESVIACIÓN ESTÁNDAR", "COEFICIENTE DE VARIACIÓN")
                        
write.xlsx(settasasalldef, file = "Última.xlsx", sheetName = "Tasas y conteos")

### Municipios con violencia crónica
source("code.R", encoding = "UTF8")
newvector <- matrix(0, nrow = 1122, ncol=1)
newvector2 <- matrix(NA, nrow = 1122, ncol=1)
newmatrix <- matrix(NA, nrow= 1122, ncol=16)
propordef <- cbind(propor[1:4], newvector, Final[5:23], newvector, Final[24:39], newvector2, newmatrix)
colnames(propordef)[5:21] <- colnames(settasasalldef[5:21])
colnames(propordef)[25:41] <- colnames(settasasalldef[25:41])
colnames(propordef)[42:58] <- colnames(settasasalldef[42:58])


for(i in settasasalldef$Código){
  propordef[propordef$`CÓDIGO-MUNICIPIO`== i,][,1:58] <- settasasalldef[settasasalldef$Código == i,][,1:58]
  
  
  }

propordef[,43:58][is.na(propordef[,43:58])] <- "Policía"

write.xlsx(propordef, file = "ÚltimaVar.xlsx", sheetName = "Tasas y conteos")
####

tipovar2 <- c("text", "text", "text", "text")

repetidos <- read_excel("Data/Repetidos.xlsx", col_types = tipovar2)
repetido <- NULL
for(i in as.character(repetidos$'Código Municipio')){
  x <- propordef %>% filter(propordef$`CÓDIGO-MUNICIPIO` == i)
  repetido <- rbind(repetido, x)
}

write.xlsx(repetido, file = "repetidos.xlsx", sheetName = "Tasas y conteos")


for(i in as.character(repetidos$'Código Municipio')){
  propordef <- propordef %>% filter(propordef$`CÓDIGO-MUNICIPIO` != i)
}
write.xlsx(propordef, file = "Sin repetidos.xlsx", sheetName = "Tasas y conteos")

