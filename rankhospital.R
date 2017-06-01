getwd()
setwd("~/GitHub/Programacion_Actuarial_III/Caso2")

rankhospital <- function(estado, resultado, n = "mejor"){
    
    datosbase<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    x <- levels(factor(datosbase[,7]))
    v <- c("ataque", "falla", "neumonia")
    
    if (estado %in% x == F){
        stop("Estado Inválido")
        break
    }
    if (resultado == "ataque") columnar <- 11
    else if (resultado == "falla") columnar <- 17
    else if (resultado == "neumonia") columnar <- 23
    else if (resultado %in% v == F){
        stop("Resultado Inválido")
        break
    }
    vector1 <- datosbase[datosbase$State == estado,]
    vector2 <- vector1[,c(2,columnar)]
    if (sum(vector2[,2]=="Not Available") < 1) {
        
        valor <- vector2[order(as.numeric(final[,2])),]
        if (n == "mejor") n <- 1
        else if (n== "peor") n <- nrow(valor)
        else if (n > nrow(valor)) {
            stop(return(NA))
        }
        i <- 0
        while (valor[i+1,2] != valor[n,2]){
            i <- i + 1
        }
        dif <- n - i
        valorl <- valor[which(valor[,2] == valor[n,2]),]
        valorr <- valorl[order(valorl[,1]),]
        valorr[dif,1] 
    }
    
    else  {
        final <- vector2[- grep("Not", vector2[,2]),]
        valor <- final[order(as.numeric(final[,2])),]
        if (n == "mejor") n <- 1
        else if (n == "peor") n <- nrow(valor)
        else if (n > nrow(valor)) {
            stop(return(NA))
        }
        i <- 0
        while (valor[i+1,2] != valor[n,2]){
            i <- i + 1
        }
        dif <- n - i
        valorl <- valor[which(valor[,2] == valor[n,2]),]
        valorr <- valorl[order(valorl[,1]),]
        valorr[dif,1]
    }
}
rankhospital("TX", "falla", 4)
rankhospital("MD", "ataque", "peor")
rankhospital("MN", "ataque", 5000)

