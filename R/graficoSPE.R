#' Gráfico SPE
#'
#' Esta función genera el gráfico SPE
#' @param x salida de score_imp()
#' @param alfa nivel de significacion
#' @param autov autovalores
#' @param A cantidad de componentes elegidas
#'
#' @return gráfico SPE
#' @details agregar detalles
#' @export
#' @examples
graficoSPE <- function(x, alfa, autov, A) {

  #pag 26 tesis arteaga
  lambdas <- autov[-(1:A)]
  tita1 <- sum(lambdas)
  tita2 <- sum(lambdas^2)
  tita3 <- sum(lambdas^3)
  h0 <- 1 - (2*tita1*tita3) / (3*tita2*tita2)
  zalfa <- qnorm(1-alfa)

  UCL1 <- zalfa * sqrt(2*tita2*h0^2)
  UCL2 <- tita2*h0*(h0-1) / tita1^2
  UCL <- tita1 * (1 + UCL1 + UCL2)^(1/h0)

  x <- x$Errores #hay que centrar??? julia no sabe
  n <- nrow(x)
  SPE <- apply(x, 1, FUN = function(x) {sum(x^2)})
  y <- data.frame(orden = 1:n, spe = SPE, control = SPE < UCL)
  z <- data.frame(x = 1, y = UCL*1.15, txt = "UCL")

  graf <- ggplot2::ggplot(data = y, aes(x = orden, y = spe)) +
    geom_line() +
    geom_point(aes(col = control)) +
    geom_hline(yintercept = UCL, col = "red", size = 1.2, linetype = "twodash") +
    geom_text(data = z, aes(x = x, y = y, label = txt)) +
    scale_color_manual(values = c("blue", "black")) +
    ggtitle("SPE Control Chart") +
    xlab("Orden") + ylab("SPE") +
    #scale_x_continuous(breaks = seq(1, n, by = 2)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(face = "bold"))

  return(graf)
}
