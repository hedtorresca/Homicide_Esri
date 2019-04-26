library(tidyverse) # Paquete para manipulación y consulta.
library(readxl) #Paquete para lectura de datos.
library(xlsx)
library(openxlsx)



#Lectura

tipovar2 <- c("numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric" , "numeric", "numeric", "numeric", "numeric", "numeric" )

Poblacion <- read_excel("ProyeccionDane.xlsx", col_types = tipovar2)


### Primer archivo

rownames <- getSheetNames("Data/Municipios1.xlsx")
  
i <- 1
municipio <- read_excel("Data/Municipios1.xlsx", 
                        sheet = i, range = "B3:D19", col_names = c("Medicina Legal", "Policía", "Fiscalía")) 


municipio <- as.data.frame(t(municipio))
major <- matrix(ncol= 17, nrow=1, 0)
fuente <- matrix(ncol= 17, nrow=1, 0)

for(i in  1:17){
  major[i] <- municipio[order(municipio[,i], decreasing = TRUE)[1],i]
  fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing=TRUE)[1]]
}

setconteos <- major
setfuentes <- fuente

for(i in  1:17){
  major[i] <- municipio[order(municipio[,i], decreasing=TRUE)[1],i]
  fuente[i]<- row.names(municipio)[order(municipio[,i], decreasing=TRUE)[1]]
}

for(i in 2:45){
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
colnames(setconteos) <- c("2002.conteo", "2003.conteo", "2004.conteo", "2005.conteo", "2006.conteo", "2007.conteo", "2008.conteo", "2009.conteo", "2010.conteo", "2011.conteo", "2012.conteo", "2013.conteo", "2014.conteo", "2015.conteo", "2016.conteo", "2017.conteo", "2018.conteo")

row.names(setfuentes) <-rownames
colnames(setfuentes) <- c("2002.fuente", "2003.fuente", "2004.fuente", "2005.fuente", "2006.fuente", "2007.fuente", "2008.fuente", "2009.fuente", "2010.fuente", "2011.fuente", "2012.fuente", "2013.fuente", "2014.fuente", "2015.fuente", "2016.fuente", "2017.fuente", "2018.fuente")


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
  settasas[settasas[,1]== i,4] <- paste0(Poblacion$MUNICIPIO[Poblacion$DPMP == i], " - ", Poblacion$DEPARTAMENTO[Poblacion$DPMP == i])
}

colnames(settasas) <- c("Código", "Municipio", "Departamento", "Municipio - Departamento")


settasas1 <- as_tibble(cbind(settasas, matrix(0, ncol=17, nrow=45), setconteos, setfuentes))

for(k in 5:21){
  for(i in t(settasas1[,1])){
    settasas1[settasas1[,1]==i,k] <- (as.numeric(settasas1[settasas1[,1]==i, k + 17])/Poblacion[Poblacion$DPMP == i,k+2])*100000
  }
  colnames(settasas1)[k] <- paste0(2000+k-3,".tasa")
}



### Segundo archivo

rownames <- getSheetNames("Data/Municipios2.xlsx")

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

for(i in 2:91){
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

row.names(setconteos) <-rownames
colnames(setconteos) <- c("2002.conteo", "2003.conteo", "2004.conteo", "2005.conteo", "2006.conteo", "2007.conteo", "2008.conteo", "2009.conteo", "2010.conteo", "2011.conteo", "2012.conteo", "2013.conteo", "2014.conteo", "2015.conteo", "2016.conteo", "2017.conteo", "2018.conteo")

row.names(setfuentes) <-rownames
colnames(setfuentes) <- c("2002.fuente", "2003.fuente", "2004.fuente", "2005.fuente", "2006.fuente", "2007.fuente", "2008.fuente", "2009.fuente", "2010.fuente", "2011.fuente", "2012.fuente", "2013.fuente", "2014.fuente", "2015.fuente", "2016.fuente", "2017.fuente", "2018.fuente")


settasas <- matrix(0, ncol = 4, nrow = 91)
for(i in 1:91){
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


settasas2 <- as_tibble(cbind(settasas, matrix(0, ncol=17, nrow=91), setconteos, setfuentes))

for(k in 5:21){
  for(i in t(settasas2[,1])){
    settasas2[settasas2[,1]==i,k] <- (as.numeric(settasas2[settasas2[,1]==i, k + 17])/Poblacion[Poblacion$DPMP == i,k+2])*100000
  }
  colnames(settasas2)[k] <- paste0(2000+k-3,".tasa")
}


### Tercer archivo

rownames <- getSheetNames("Data/Municipios3.xlsx")

i <- 1
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

for(i in 2:35){
  municipio <- read_excel("Data/Municipios3.xlsx", 
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
colnames(setconteos) <- c("2002.conteo", "2003.conteo", "2004.conteo", "2005.conteo", "2006.conteo", "2007.conteo", "2008.conteo", "2009.conteo", "2010.conteo", "2011.conteo", "2012.conteo", "2013.conteo", "2014.conteo", "2015.conteo", "2016.conteo", "2017.conteo", "2018.conteo")

row.names(setfuentes) <-rownames
colnames(setfuentes) <- c("2002.fuente", "2003.fuente", "2004.fuente", "2005.fuente", "2006.fuente", "2007.fuente", "2008.fuente", "2009.fuente", "2010.fuente", "2011.fuente", "2012.fuente", "2013.fuente", "2014.fuente", "2015.fuente", "2016.fuente", "2017.fuente", "2018.fuente")


settasas <- matrix(0, ncol = 4, nrow = 35)
for(i in 1:35){
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


settasas3 <- as_tibble(cbind(settasas, matrix(0, ncol=17, nrow=35), setconteos, setfuentes))

for(k in 5:21){
  for(i in t(settasas3[,1])){
    settasas3[settasas3[,1]==i,k] <- (as.numeric(settasas3[settasas3[,1]==i, k + 17])/Poblacion[Poblacion$DPMP == i,k+2])*100000
  }
  colnames(settasas3)[k] <- paste0(2000+k-3,".tasa")
}




### Cuarto archivo

rownames <- getSheetNames("Data/Municipios4.xlsx")

i <- 1
municipio <- read_excel("Data/Municipios4.xlsx", 
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

for(i in 2:128){
  municipio <- read_excel("Data/Municipios4.xlsx", 
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
colnames(setconteos) <- c("2002.conteo", "2003.conteo", "2004.conteo", "2005.conteo", "2006.conteo", "2007.conteo", "2008.conteo", "2009.conteo", "2010.conteo", "2011.conteo", "2012.conteo", "2013.conteo", "2014.conteo", "2015.conteo", "2016.conteo", "2017.conteo", "2018.conteo")

row.names(setfuentes) <-rownames
colnames(setfuentes) <- c("2002.fuente", "2003.fuente", "2004.fuente", "2005.fuente", "2006.fuente", "2007.fuente", "2008.fuente", "2009.fuente", "2010.fuente", "2011.fuente", "2012.fuente", "2013.fuente", "2014.fuente", "2015.fuente", "2016.fuente", "2017.fuente", "2018.fuente")


settasas <- matrix(0, ncol = 4, nrow = 128)
for(i in 1:128){
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


settasas4 <- as_tibble(cbind(settasas, matrix(0, ncol=17, nrow=128), setconteos, setfuentes))

for(k in 5:21){
  for(i in t(settasas4[,1])){
    settasas4[settasas4[,1]==i,k] <- (as.numeric(settasas4[settasas4[,1]==i, k + 17])/Poblacion[Poblacion$DPMP == i,k+2])*100000
  }
  colnames(settasas4)[k] <- paste0(2000+k-3,".tasa")
}

### Quinto archivo

rownames <- getSheetNames("Data/Municipios5.xlsx")

i <- 1
municipio <- read_excel("Data/Municipios5.xlsx", 
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

for(i in 2:37){
  municipio <- read_excel("Data/Municipios5.xlsx", 
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
colnames(setconteos) <- c("2002.conteo", "2003.conteo", "2004.conteo", "2005.conteo", "2006.conteo", "2007.conteo", "2008.conteo", "2009.conteo", "2010.conteo", "2011.conteo", "2012.conteo", "2013.conteo", "2014.conteo", "2015.conteo", "2016.conteo", "2017.conteo", "2018.conteo")

row.names(setfuentes) <-rownames
colnames(setfuentes) <- c("2002.fuente", "2003.fuente", "2004.fuente", "2005.fuente", "2006.fuente", "2007.fuente", "2008.fuente", "2009.fuente", "2010.fuente", "2011.fuente", "2012.fuente", "2013.fuente", "2014.fuente", "2015.fuente", "2016.fuente", "2017.fuente", "2018.fuente")


settasas <- matrix(0, ncol = 4, nrow = 37)
for(i in 1:37){
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


settasas5 <- as_tibble(cbind(settasas, matrix(0, ncol=17, nrow=37), setconteos, setfuentes))

for(k in 5:21){
  for(i in t(settasas5[,1])){
    settasas5[settasas5[,1]==i,k] <- (as.numeric(settasas5[settasas5[,1]==i, k + 17])/Poblacion[Poblacion$DPMP == i,k+2])*100000
  }
  colnames(settasas5)[k] <- paste0(2000+k-3,".tasa")
}

# Sexto archivo
  

  rownames <- getSheetNames("Data/Municipios6.xlsx")
  
  i <- 1
  municipio <- read_excel("Data/Municipios6.xlsx", 
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
  
  for(i in 2:length(rownames)){
    municipio <- read_excel("Data/Municipios6.xlsx", 
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
  colnames(setconteos) <- c("2002.conteo", "2003.conteo", "2004.conteo", "2005.conteo", "2006.conteo", "2007.conteo", "2008.conteo", "2009.conteo", "2010.conteo", "2011.conteo", "2012.conteo", "2013.conteo", "2014.conteo", "2015.conteo", "2016.conteo", "2017.conteo", "2018.conteo")
  
  row.names(setfuentes) <-rownames
  colnames(setfuentes) <- c("2002.fuente", "2003.fuente", "2004.fuente", "2005.fuente", "2006.fuente", "2007.fuente", "2008.fuente", "2009.fuente", "2010.fuente", "2011.fuente", "2012.fuente", "2013.fuente", "2014.fuente", "2015.fuente", "2016.fuente", "2017.fuente", "2018.fuente")
  
  
  settasas <- matrix(0, ncol = 4, nrow = length(rownames))
  for(i in 1:length(rownames)){
    settasas[i,1] <- rownames[i]
  }
  
  
  for(i in t(settasas[,1])){
    settasas[settasas[,1]== i,2] <- Poblacion$MUNICIPIO[Poblacion$DPMP == i]
  }
  i
  
  for(i in t(settasas[,1])){
    settasas[settasas[,1]== i,3] <- Poblacion$DEPARTAMENTO[Poblacion$DPMP == i]
  }
  
  
  for(i in t(settasas[,1])){
    settasas[settasas[,1]== i,4] <- paste0(Poblacion$MUNICIPIO[Poblacion$DPMP == i], " - ", Poblacion$DEPARTAMENTO[Poblacion$DPMP == i])
  }
  
  colnames(settasas) <- c("Código", "Municipio", "Departamento", "Municipio - Departamento")
  
  
  settasas6 <- as_tibble(cbind(settasas, matrix(0, ncol=17, nrow=length(rownames)), setconteos, setfuentes))
  
  for(k in 5:21){
    for(i in t(settasas6[,1])){
      settasas6[settasas6[,1]==i,k] <- (as.numeric(settasas6[settasas6[,1]==i, k + 17])/Poblacion[Poblacion$DPMP == i,k+2])*100000
    }
    colnames(settasas6)[k] <- paste0(2000+k-3,".tasa")
  
  
  
}
settasasall <- unique(rbind(settasas1,settasas2, settasas3, settasas4, settasas5, settasas6))

settasasall$promedio.tasa <- apply(apply(select (settasasall, contains (".tasa")),2,as.numeric),1,mean) 
settasasall$sd.tasa <- apply(apply(select (settasasall, contains (".tasa")),2,as.numeric),1,sd)
settasasall$cv.tasa <- 100*settasasall$sd.tasa/settasasall$promedio.tasa

write.xlsx(settasasall, file = "Última.xlsx", sheetName = "Tasas y conteos")


### Municipios con violencia crónica
source("code.R")

for(i in settasasall$Código){
  propor2
}