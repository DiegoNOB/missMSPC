#' SPE Control Chart
#'
#' This function plots the SPE Control Chart.
#' It also returns the proportion of out-of-control samples and their position,
#' along with the value of the computed Upper Control Limit.
#' @param scores output from `score_imp()` function.
#' @param alpha significance level used for computing the Upper Control Limit (UCL).
#' Defaults to 0.005.
#'
#' @return `spe.chart` returns a list containing the following components:
#' \describe{
#'   \item{Plot}{ggplot object with SPE control chart}
#'   \item{poc}{Proportion of out-of-control samples}
#'   \item{out}{ID of out-of-control samples}
#'   \item{UCL}{value of the Upper Control Limit}
#' }
#' @details The calculation of the Upper Control Limit is based on Jackson & Mudholkar (1979).
#' @export
#' @references Jackson, J. E., & Mudholkar, G. S. (1979).
#' "Control Procedures for Residuals Associated with Principal Component Analysis".
#' Technometrics, 21(3), 341-349. <https://doi.org/10.2307/1267757>
#'
#' MacGregor, J. F., & Kourti, T. (1995).
#' "Statistical process control of multivariate processes."
#' Control engineering practice, 3(3), 403-414.
#'
#' @seealso [score_imp()]
#' @examples
#' #Score imputation with CMR method
#' scores <- score_imp(
#'   data1 = fruits1,
#'   data2 = fruits2na,
#'   A = 4,
#'   method = "CMR"
#'   )
#' spe.chart(scores = scores, alpha = 0.005)
spe.chart <- function(scores, alpha = 0.005) {

  #pag 26 tesis arteaga
  autov <- scores$VarScores
  A <- ncol(scores$Scores)
  lambdas <- autov[-(1:A)]
  tita1 <- sum(lambdas)
  tita2 <- sum(lambdas^2)
  tita3 <- sum(lambdas^3)
  h0 <- 1 - (2*tita1*tita3) / (3*tita2*tita2)
  zalpha <- qnorm(1-alpha)

  UCL1 <- zalpha * sqrt(2 * tita2 * h0^2)
  UCL2 <- tita2 * h0 * (h0 - 1) / tita1^2
  UCL <- tita1 * (1 + UCL1 + UCL2)^(1/h0)

  x <- scores$Residuals
  n <- nrow(x)
  SPE <- apply(x, 1, FUN = function(x) {sum(x^2)})
  y <- data.frame(orden = 1:n, spe = SPE, control = SPE < UCL)

  graf <- ggplot2::ggplot(data = y) +
    ggplot2::aes(x = orden, y = spe) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(ggplot2::aes(col = control), size = 2) +
    ggplot2::geom_hline(yintercept = UCL, col = "blue", size = 1.2, linetype = "twodash") +
    ggplot2::geom_text(ggplot2::aes(x = 1, y = UCL*1.05, label = "UCL")) +
    ggplot2::scale_color_manual(breaks = c(FALSE, TRUE),
                                values = c("red", "black")) +
    ggplot2::ggtitle("SPE Control Chart") +
    ggplot2::xlab("Order") +
    ggplot2::ylab("SPE") +
    #scale_x_continuous(breaks = seq(1, n, by = 2)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      axis.title = ggplot2::element_text(face = "bold")
      )

  pfc <- mean(!y$control) #Proporcion de obs fuera de control
  obsfc <- which(!y$control) #ID de obs fuera de control
  if (length(obsfc) == 0) {obsfc <- NULL}

  return(list(Plot = graf, poc = pfc, out = obsfc, UCL = UCL))
}
