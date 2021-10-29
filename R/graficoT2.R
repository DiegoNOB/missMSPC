#' Gráfico T^2
#'
#' Esta función genera el gráfico T^2
#' @param x salida de la función cmr_tsr()
#' @param alfa nivel de significacion
#'
#' @return devuelve una lista con los siguientes elementos: i) gráfico T^2 generado, ii) proporción de observaciones fuera de control, iii) ID de observaciones fuera de control
#' @details agregar detalles
#' @export
#' @examples
graficoT2 <- function(x, alfa) {

  x <- x$Scores

  #julia no tiene claro que n deberia ser
  #opcion 1) el del conjunto de datos que use para ajustar el modelo PCA
  #opcion 2) el del conjunto de datos del proceso que estoy controlando
  n <- nrow(x) #opcion 2
  A <- ncol(x)

  if (n <= A) {stop(stringr::str_wrap(paste0("La cantidad de filas (", n,
                                    ") no puede ser menor o igual a la cantidad de componentes (", A, ")."), width = 75))}

  UCL <- qf(1-alfa, df1 = A, df2 = n-A) * A * (n+1) * (n-1) / (n * (n-A)) #UPPER CONTROL LIMIT

  titahat <- var(x) #matriz estimada de var y cov de los scores. la real es TITA
  tauC <- x
  #tauC <- scale(x, scale = F) #scores centrados (media 0) mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

  salida <- NULL
  for (j in 1:nrow(tauC)) {
    filaj <- tauC[j, ]
    estj <- filaj %*% solve(titahat) %*% matrix(filaj, ncol = 1) #T2 para cada fila
    salida <- c(salida, estj)
  }

  y <- data.frame(orden = 1:n, t2 = salida, control = salida < UCL)
  z <- data.frame(x = 1, y = UCL*1.05, txt = "UCL")

  graf <- ggplot2::ggplot(data = y, aes(x = orden, y = t2)) +
    geom_line() +
    geom_point(aes(col = control)) +
    geom_hline(yintercept = UCL, col = "red", size = 1.2, linetype = "twodash") +
    geom_text(data = z, aes(x = x, y = y, label = txt)) +
    scale_color_manual(values = c("blue", "black")) +
    ggtitle(expression(paste("T"^"2", " Control Chart"))) +
    xlab("Orden") + ylab(expression(paste("T"^"2"))) +
    #scale_x_continuous(breaks = seq(1, n, by = 2)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(face = "bold"))

  pfc <- mean(!y$control) #Proporcion de obs fuera de control
  obsfc <- which(!y$control) #ID de obs fuera de control
  if (length(obsfc) == 0) {obsfc <- NULL}

  return(list(Plot = graf, pfc = pfc, out = obsfc))

}
