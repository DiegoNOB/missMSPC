#' Imputación de Scores CMR y TSR
#'
#' Esta función calcula scores usando el metodo CMR (KDR) o TSR
#' @param datos vector o matriz de datos numericos que pueden tener missing values
#' @param pesos matriz de pesos que sale del PCA
#' @param autov autovalores
#' @param A cantidad de componentes elegidas
#' @param metodo CMR o TSR
#'
#' @return devuelve una lista con scores y errores calculados
#' @details agregar detalles
#' @export
#' @examples
#'
score_imp <- function(datos, pesos, autov, A, metodo) {

  if (!(metodo %in% c("CMR", "TSR"))) {
    stop(stringr::str_wrap("Los únicos métodos aceptados son CMR o TSR.", width = 75))
  }

  #supongo que la dimension sera nula solo si el objeto es un vector
  #las clases mas comunes (matrix, data.frame, tibble) tienen dimension
  #chequear si otras clases similares (no se cuales) tienen dim nula o no
  if (is.null(dim(datos))) {datos <- matrix(datos, nrow = 1)} #Convierto vector a matriz
  datos <- as.matrix(datos) #para el caso data.frame o tibble

  #Cantidad de variables y chequeos
  p <- ncol(datos)

  if (nrow(pesos) != p) {stop(stringr::str_wrap(paste0("La cantidad de variables en la matriz de pesos (",
                                              nrow(pesos), ") no coincide con la cantidad de variables del conjunto de datos (",
                                              p, ")."), width = 75))}

  if (ncol(pesos) < A) {stop(stringr::str_wrap(paste0("La cantidad de componentes en la matriz de pesos (",
                                             ncol(pesos), ") es menor a la cantidad de componentes elegidas (", A, ")."), width = 75))}

  if (length(autov) < A) {stop(stringr::str_wrap(paste0("La cantidad de autovalores (", length(autov),
                                               ") es menor a la cantidad de componentes elegidas (", A, ")."), width = 75))}

  TITA <- diag(autov) #Matriz de covariancia de las componentes
  P1A <- as.matrix(pesos[, 1:A]) #Pesos para las A primeras componentes

  escores <- NULL
  errores <- NULL

  for (j in 1:nrow(datos)) {

    z <- datos[j, ]
    faltan <- which(is.na(z)) #positions of missing data
    cantMV <- length(faltan) #cantidad de missing values

    #Escenario 1: no hay datos faltantes, los scores se calculan con la formula usual
    if (cantMV == 0) {
      tauhat <- t(z %*% P1A)
      residuo <- (diag(p) - P1A %*% t(P1A)) %*% z
    }

    #Escenario 2: todas las variables tienen datos faltantes, imposible estimar el score
    else if (cantMV == p) {
      warning(stringr::str_wrap(paste0("La totalidad de las variables presentan valores faltantes
           para la observación (fila) ", j, "."), width = 75))
      tauhat <- rep(NA, A)
      residuo <- rep(NA, p)
    }

    #Escenario 3: solo algunas variables tienen datos faltantes, aplico CMR o TSR
    else {

      zstar <- z[-faltan] #z*: vector of observed values
      Pstar <- matrix(pesos[-faltan, ], ncol = p) #P*: weight matrix of obs variables
      Pstar1A <- matrix(Pstar[, 1:A], ncol = A) #P*1:A: pesos de A PCs of obs variables
      Phash <- matrix(pesos[faltan, ], ncol = p)

      if (metodo == "CMR") {

        #Scores Estimados
        inversa <- solve(Pstar %*% TITA %*% t(Pstar))
        tauhat <- TITA[1:A, 1:A] %*% t(Pstar1A) %*% inversa %*% zstar #score con valores imputados

        #Errores Estimados (justificacion de usar 2.33: pag 109 tesis arteaga)
        zhash <- Phash %*% TITA %*% t(Pstar) %*% inversa %*% zstar #formula 2.33 tesis arteaga

      }

      else if (metodo == "TSR") {

        #Scores Estimados
        inversa <- solve(t(Pstar1A) %*% Pstar %*% TITA %*% t(Pstar) %*% Pstar1A)
        tauhat <- TITA[1:A, 1:A] %*% t(Pstar1A) %*% Pstar1A %*% inversa %*% t(Pstar1A) %*% zstar #score con valores imputados

        #Errores Estimados
        taustar1A <- t(Pstar1A) %*% zstar #pag 88 tesis arteaga
        zhash <- Phash %*% TITA %*% t(Pstar) %*% Pstar1A %*% inversa %*% taustar1A #formula 3.18 tesis arteaga

      }

      zimpu <- c(zhash, zstar)[order(c(faltan, setdiff(1:p, faltan)))] #z imputados
      residuo <- (diag(p) - P1A %*% t(P1A)) %*% zimpu

    }

    escores <- rbind(escores, t(tauhat))
    errores <- rbind(errores, t(residuo))
  }

  return(list(Scores = escores, Errores = errores))
}
