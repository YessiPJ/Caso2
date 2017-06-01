getwd()
setwd("~/GitHub/Programacion_Actuarial_III/Caso2")

mejor <- function(estado,resultado){
    ##Cargar datos
    datosbase <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    x <- levels(factor(datosbase[,7]))
    v <- c("ataque", "falla", "neumonia")
    
    ##Vector Lógico
    if (estado %in% x == F){
        stop("Estado inválido")
        break
    }
    ## Generar columnas comparativas
    if (resultado == "ataque") columnar <- 11
    else if (resultado == "falla") columnar <- 17
    else if (resultado == "neumonia") columnar <- 23
    else if (resultado %in% v == F){
        stop("Resultado inválido")
        break
    }
    ##extracción de datos
    vector1<- datosbase[datosbase$State == estado,]
    vector2 <- vector1[,c(2,columnar)]
    
    if (sum(vector2[,2]=="Not Available") < 1) {
        ##ordena
        valor <- vector2[order(as.numeric(vector2[,2])),]
        ##asignacion valor logico
        valorl <- valor[which(valor[,2] == valor[1,2]),]
        valorr <- valor[order(valorl[,1]),]
        valorr[1,1]
        
    }
    else {
        final <- vector2[- grep("Not", vector2[,2]),]
        valor <- final[order(as.numeric(final[,2])),]
        valorl <- valor[which(valor[,2] == valor[1,2]),]
        valorr <- valorl[order(valorl[,1]),]
        valorr[1,1]
    }
}

mejor("TX", "ataque")
mejor("TX", "falla")
mejor("MD", "ataque")
mejor("MD", "neumonia")
mejor("BB", "ataque")
mejor("NY", "infartu")

