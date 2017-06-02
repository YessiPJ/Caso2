getwd()
setwd("~/GitHub/Programacion_Actuarial_III/Caso2")

rankhospital <- function(estado, resultado, num= "mejor"){
    ##Cargar datos
    datosbase<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    x <- levels(factor(datosbase[,7]))
    v <- c("ataque", "falla", "neumonia")
    ##Vector lógico
    if (estado %in% x == F){
        stop("Estado Inválido")
        break
    }
    ##obtención columna
    if (resultado == "ataque") columnar <- 11
    else if (resultado == "falla") columnar <- 17
    else if (resultado == "neumonia") columnar <- 23
    else if (resultado %in% v == F){
        stop("Resultado Inválido")
        break
    }
    #extracción de datos
    vector1 <- datosbase[datosbase$State == estado,]
    vector2 <- vector1[,c(2,columnar)]
    
    #
    if (sum(vector2[,2]=="Not Available") < 1) {
        
        ##Condicion no rebase el n
        
        valor <- vector2[order(as.numeric(valor2[,2])),]
        if (num== "mejor") num <- 1
        else if (num== "peor") num <- nrow(valor)
        else if (num > nrow(valor)) {
            stop(return(NA))
        }
        "comparacion diferente"
        i <- 0
        while (valor[i+1,2] != valor[num,2]){
            i <- i + 1
        }
        #guarda valores
        dif <- num - i
        valorl <- valor[which(valor[,2] == valor[num,2]),]
        #ordena y arroja resultado
        valorr <- valorl[order(valorl[,1]),]
        valorr[dif,1] 
    }
    
    else  {
        #coincidencia valor logico
        valor2 <- vector2[- grep("Not", vector2[,2]),]
        #ordena
        valor <- valor2[order(as.numeric(valor2[,2])),]
        if (num == "mejor") num <- 1
        else if (num == "peor") num <- nrow(valor)
        else if (num > nrow(valor)) {
            stop(return(NA))
        }
        i <- 0
        while (valor[i+1,2] != valor[num,2]){
            i <- i + 1
        }
        dif <- num - i
        valorl <- valor[which(valor[,2] == valor[num,2]),]
        valorr <- valorl[order(valorl[,1]),]
        valorr[dif,1]
    }
}
rankhospital("TX", "falla", 4)
rankhospital("MD", "ataque", "peor")
rankhospital("MN", "ataque", 5000)

