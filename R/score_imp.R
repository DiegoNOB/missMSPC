#' PCA Score Calculation
#'
#' This function calculates the scores and residuals for the \eqn{T^2} control chart
#' of Principal Components. For Phase II data, the scores of observations with missing values
#' are estimated using either CMR or TSR methods.
#' @param data1 an optional argument: data frame with Phase I data.
#' Cannot contain missing values.
#' @param data2 data frame with Phase II data. May contain missing values.
#' @param A number of principal components to use.
#' @param method method to be used for imputation of missing values, if there are any.
#' Currently supported values are "CMR" and "TSR".
#' @param weights an optional argument: weight matrix from Principal Component Analysis,
#' computed using standardized Phase I data.
#' @param eigen an optional argument: numeric vector with eigenvalues
#' from Principal Component Analysis, computed using standardized Phase I data.
#'
#' @return `score_imp` returns a list containing the following components:
#' \describe{
#'   \item{Scores}{scores for Phase II data for the first `A` principal components}
#'   \item{Residuals}{residuals from the model based on the first A principal components}
#'   \item{VarScores}{eigenvalues from PCA using standardized Phase I data}
#' }
#' @details For Phase I usage, `data2` should be equal to `data1`.
#' Phase II data is standardized using means and variances from Phase I data.
#' If Phase I data is not provided, `score_imp` expects `data2` to be standardized.
#' In this case, user should provide both `weights` and `eigen`.
#'
#' When phase I data is available and scores are estimated through the TSR method,
#' problems arising from matrix singularity are dealt with Ridge regression.
#'
#' @references Arteaga, F., & Ferrer, A. (2002).
#' "Dealing with Missing Data in MSPC: Several Methods, Different Interpretations, Some Examples".
#' Journal of Chemometrics 16 (810): 408-18. <https://doi.org/10.1002/cem.750>
#'
#' Fernández, J. I.; Pagura, J. A. & Quaglino, M. B. (2021).
#' "Assessment of the effect of imputation of missing values on the performance of Phase II multivariate control charts".
#' Quality and Reliability Engineering International, 37(4), 1664-1677.
#' <https://doi.org/10.1002/qre.2819>
#'
#' Nelson, P. R. C.; Taylor, P. A. & MacGregor, J. F. (1996).
#' "Missing Data Methods in PCA and PLS: Score Calculations with Incomplete Observations".
#' Chemometrics and Intelligent Laboratory Systems 35 (1): 45-65.
#' <https://doi.org/https://doi.org/10.1016/S0169-7439(96)00007-X>

#' @export
#' @examples
#' #Score calculation
#' score_imp(
#'   data1 = fruits1,
#'   data2 = fruits2,
#'   A = 4
#'   )
#'
#' #Score imputation with CMR method
#' score_imp(
#'   data1 = fruits1,
#'   data2 = fruits2na,
#'   A = 4,
#'   method = "CMR"
#'   )
score_imp <- function(data1 = NULL, data2, A, method = NULL, weights = NULL, eigen = NULL) {

  if (!is.null(data1)) {
    data2_est <- scale(
      data2,
      center = colMeans(data1),
      scale = sqrt(diag(var(data1)))
      )
  }

  if (is.null(data1) & (is.null(weights) | is.null(eigen))) {
    stop("If Phase I data is not provided, both weights and eigen are necessary")
  }

  if (is.null(data1) & !is.null(weights) & !is.null(eigen)) {
    message("Phase II data is assumed to be standardized using means and variances from Phase I data.")
  }

  if (is.null(weights) | is.null(eigen)) {
    data1_est <- scale(data1)
    acp <- stats::princomp(data1_est)
    eigen <- acp$sdev^2
    weights <- matrix(data = as.numeric(acp$loadings), ncol = length(eigen))
  }

  #Cantidad de variables y chequeos
  p <- ncol(data2)

  if (nrow(weights) != p) {
    stop(paste0("The number of variables in the weights matrix (",
                nrow(weights),
                ") is different from the number of columns in Phase II data (",
                p, ")."))}

  if (ncol(weights) < A) {
    stop(paste0("The number of components in the weights matrix (",
                  ncol(weights),
                 ") is less than the number of selected components (", A, ")."))
         }

  if (length(eigen) < A) {
    stop(paste0("The number of eigenvalues (",
                length(eigen),
                ") is less than the number of selected components (", A, ")."))
         }

  TITA <- diag(eigen) #Matriz de covariancia de las componentes
  P1A <- as.matrix(weights[, 1:A]) #weights para las A primeras componentes

  escores <- NULL
  errores <- NULL

  for (j in 1:nrow(data2)) {

    z <- as.matrix(data2[j, ])
    faltan <- which(is.na(z)) #positions of missing data
    cantMV <- length(faltan) #cantidad de missing values

    #Escenario 1: no hay datos faltantes, los scores se calculan con la formula usual
    if (cantMV == 0) {
      tauhat <- t(z %*% P1A)
      residuo <- (diag(p) - P1A %*% t(P1A)) %*% t(z)
    }

    #Escenario 2: todas las variables tienen datos faltantes, imposible estimar el score
    else if (cantMV == p) {
      warning(paste0("Missing values detected in all variables. ",
                     "Score imputation methods cannot be implemented in this scenario.",
                     "Check row ", j, "."))
      tauhat <- rep(NA, A)
      residuo <- rep(NA, p)
    }

    #Escenario 3: solo algunas variables tienen datos faltantes, aplico CMR o TSR
    else {

      if (is.null(method)) {stop("Choose method CMR or TSR.")}

      if (!(method %in% c("CMR", "TSR"))) {
        stop("Choose method CMR or TSR.")
      }

      zstar <- z[-faltan] #z*: vector of observed values
      Pstar <- matrix(weights[-faltan, ], ncol = p) #P*: weight matrix of obs variables
      Pstar1A <- matrix(Pstar[, 1:A], ncol = A) #P*1:A: weights de A PCs of obs variables
      Phash <- matrix(weights[faltan, ], ncol = p)

      if (method == "CMR") {

        #Scores Estimados
        inversa <- solve(Pstar %*% TITA %*% t(Pstar))
        tauhat <- TITA[1:A, 1:A] %*% t(Pstar1A) %*% inversa %*% zstar #score con valores imputados

        #Errores Estimados (justificacion de usar 2.33: pag 109 tesis arteaga)
        zhash <- Phash %*% TITA %*% t(Pstar) %*% inversa %*% zstar #formula 2.33 tesis arteaga

      }

      else if (method == "TSR") {

        #Scores Estimados
        auxi <- t(Pstar1A) %*% Pstar %*% TITA %*% t(Pstar) %*% Pstar1A
        condicion <- qr(auxi)$rank < A

        if (condicion & !is.null(data1)) {

          #ridge (solo si hay datos en fase 1)

          rta <- acp$scores[, 1:A] #scores con datos completos
          expli <- data1_est[,-faltan] %*% Pstar1A

          Ridge <- glmnet::glmnet(x = expli, y = rta, family = "mgaussian",
                          alpha = 0, intercept = FALSE)

          valcruz <- glmnet::cv.glmnet(x = expli, y = rta, family = "mgaussian",
                               alpha = 0, intercept = FALSE)

          Lambda <- valcruz$lambda.min
          coeficientes <- coef(Ridge, s = Lambda)
          Bhat <- as.matrix(do.call(cbind, coeficientes))[-1,]
          tauhat <- Bhat %*% t(Pstar1A) %*% zstar

        } else {
          inversa <- solve(auxi)
          #score con valores imputados
          tauhat <- TITA[1:A, 1:A] %*% t(Pstar1A) %*% Pstar1A %*% inversa %*% t(Pstar1A) %*% zstar
        }

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

  return(list(Scores = escores, Residuals = errores, VarScores = eigen))
}
