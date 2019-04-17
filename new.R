library(tidyverse) # Paquete para manipulación y consulta.
library(readxl) #Paquete para lectura de datos.
library(xlsx)
library(openxlsx)



#Lectura

tipovar2 <- c("numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric" , "numeric", "numeric", "numeric", "numeric", "numeric" )

Poblacion <- read_excel("ProyeccionDane.xlsx", col_types = tipovar2)


### Primer archivo

rownames <- getSheetNames("Data/Municipios1.xlsx")[2:46]
  
i <- 2
municipio <- read_excel("Data/Municipios1.xlsx", 
                        sheet = i, range = "B3:D19", col_names = c("Medicina Legal", "Policía", "Fiscalía")) 


municipio <- as.data.frame(t(municipio))
major <- matrix(ncol= 17, nrow=1, 0)
fuente <- matrix(ncol= 17, nrow=1, 0)

for(i in  1:17){
  major[i] <- municipio[order(municipio[,i], decreasing=TRUE)[1],i]
  fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing=TRUE)[1]]
}

setconteos <- major
setfuentes <- fuente

for(i in  1:17){
  major[i] <- municipio[order(municipio[,i], decreasing=TRUE)[1],i]
  fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing=TRUE)[1]]
}

for(i in 3:46){
    municipio <- read_excel("Data/Municipios1.xlsx", 
                         sheet = i, range = "B3:D19", col_names = c("Medicina Legal", "Policía", "Fiscalía")) 
 
    municipio <- as.data.frame(t(municipio))
    major <- matrix(ncol= 17, nrow=1, 0)
    fuente <- matrix(ncol= 17, nrow=1, 0)
    
  for(i in  1:17){
    major[i] <- municipio[order(municipio[,i], decreasing=TRUE)[1],i]
    fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing=TRUE)[1]]
  }
    setconteos <- rbind(setconteos, major)
    setfuentes <- rbind(setfuentes, fuente)
    
}

row.names(setconteos) <-rownames
colnames(setconteos) <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

row.names(setfuentes) <-rownames
colnames(setfuentes) <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")


settasas <- matrix(0, ncol = 4, nrow = 45)
for(i in 1:45){
  settasas[i,1] <- rownames[i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,2] <- Poblacion$MUNICIPIO[Poblacion$DPMP == i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,3] <- Poblacion$DEPARTAMENTO[Poblacion$DPMP == i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,4] <- paste0(Poblacion$MUNICIPIO[Poblacion$DPMP == i], "-", Poblacion$DEPARTAMENTO[Poblacion$DPMP == i])
}

settasas <- cbind(settasas, setconteos, setfuentes)


settasas1 <- as_tibble(cbind(settasas, matrix(0, ncol=17, nrow=45)))

for(k in 39:55){
  for(i in t(settasas[,1])){
    settasas1[settasas1[,1]==i,k] <- (as.numeric(settasas[settasas[,1]==i, k-34])/Poblacion[Poblacion$DPMP == i,k-32])*100000
  }
}



### Segundo archivo

rownames <- getSheetNames("Data/Municipios2.xlsx")[2:91]

i <- 1
municipio <- read_excel("Data/Municipios2.xlsx", 
                        sheet = i, range = "B3:D19", col_names = c("Medicina Legal", "Policía", "Fiscalía")) 


municipio <- as.data.frame(t(municipio))
major <- matrix(ncol= 17, nrow=1, 0)
fuente <- matrix(ncol= 17, nrow=1, 0)

for(i in  1:17){
  major[i] <- municipio[order(municipio[,i], decreasing=TRUE)[1],i]
  fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing=TRUE)[1]]
}

setconteos <- major
setfuentes <- fuente

for(i in  1:17){
  major[i] <- municipio[order(municipio[,i], decreasing=TRUE)[1],i]
  fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing=TRUE)[1]]
}

for(i in 2:90){
  municipio <- read_excel("Data/Municipios2.xlsx", 
                          sheet = i, range = "B3:D19", col_names = c("Medicina Legal", "Policía", "Fiscalía")) 
  
  municipio <- as.data.frame(t(municipio))
  major <- matrix(ncol= 17, nrow=1, 0)
  fuente <- matrix(ncol= 17, nrow=1, 0)
  
  for(i in  1:17){
    major[i] <- municipio[order(municipio[,i], decreasing=TRUE)[1],i]
    fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing=TRUE)[1]]
  }
  setconteos <- rbind(setconteos, major)
  setfuentes <- rbind(setfuentes, fuente)
  
}

colnames(setconteos) <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
colnames(setfuentes) <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

settasas <- matrix(0, ncol = 4, nrow = 90)
for(i in 1:90){
  settasas[i,1] <- rownames[i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,2] <- Poblacion$MUNICIPIO[Poblacion$DPMP == i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,3] <- Poblacion$DEPARTAMENTO[Poblacion$DPMP == i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,4] <- paste0(Poblacion$MUNICIPIO[Poblacion$DPMP == i], "-", Poblacion$DEPARTAMENTO[Poblacion$DPMP == i])
}

settasas <- cbind(settasas, setconteos, setfuentes)

# tasas <-matrix(0, ncol=17, nrow=45)
# for(k in 5:21){
#   for(i in 1:45){
#     tasas[i,1] = (as.numeric(settasas[i, k])/Poblacion[Poblacion$DPMP == settasas[i,1],k+2])*100000
#   }
# }

settasas2 <- as_tibble(cbind(settasas, matrix(0, ncol=17, nrow=90)))

for(k in 39:55){
  for(i in t(settasas[,1])){
    settasas2[settasas2[,1]==i,k] <- (as.numeric(settasas[settasas[,1]==i, k-34])/Poblacion[Poblacion$DPMP == i,k-32])*100000
  }
}

### Tercer archivo

rownames <- getSheetNames("Data/Municipios3.xlsx")[2:36]

i <- 2
municipio <- read_excel("Data/Municipios3.xlsx", 
                        sheet = i, range = "B3:D19", col_names = c("Medicina Legal", "Policía", "Fiscalía")) 


municipio <- as.data.frame(t(municipio))
major <- matrix(ncol= 17, nrow=1, 0)
fuente <- matrix(ncol= 17, nrow=1, 0)

for(i in  1:17){
  major[i] <- municipio[order(municipio[,i], decreasing=TRUE)[1],i]
  fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing=TRUE)[1]]
}

setconteos <- major
setfuentes <- fuente

for(i in  1:17){
  major[i] <- municipio[order(municipio[,i], decreasing=TRUE)[1],i]
  fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing=TRUE)[1]]
}

for(i in 3:37){
  municipio <- read_excel("Data/Municipios2.xlsx", 
                          sheet = i, range = "B3:D19", col_names = c("Medicina Legal", "Policía", "Fiscalía")) 
  
  municipio <- as.data.frame(t(municipio))
  major <- matrix(ncol= 17, nrow=1, 0)
  fuente <- matrix(ncol= 17, nrow=1, 0)
  
  for(i in  1:17){
    major[i] <- municipio[order(municipio[,i], decreasing=TRUE)[1],i]
    fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing=TRUE)[1]]
  }
  setconteos <- rbind(setconteos, major)
  setfuentes <- rbind(setfuentes, fuente)
  
}

colnames(setconteos) <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
colnames(setfuentes) <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

settasas <- matrix(0, ncol = 4, nrow = 36)
for(i in 1:36){
  settasas[i,1] <- rownames[i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,2] <- Poblacion$MUNICIPIO[Poblacion$DPMP == i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,3] <- Poblacion$DEPARTAMENTO[Poblacion$DPMP == i]
}


for(i in t(settasas[,1])){
  settasas[settasas[,1]== i,4] <- paste0(Poblacion$MUNICIPIO[Poblacion$DPMP == i], "-", Poblacion$DEPARTAMENTO[Poblacion$DPMP == i])
}

settasas <- cbind(settasas, setconteos, setfuentes)

settasas2 <- as_tibble(cbind(settasas, matrix(0, ncol=17, nrow=36)))

for(k in 39:55){
  for(i in t(settasas[,1])){
    settasas2[settasas2[,1]==i,k] <- (as.numeric(settasas[settasas[,1]==i, k-34])/Poblacion[Poblacion$DPMP == i,k-32])*100000
  }
}