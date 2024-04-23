#' \eqn{T^2} Control Chart of PCs
#'
#' This function plots the \eqn{T^2} Control Chart of PCs.
#' It also returns the proportion of out-of-control samples and their position,
#' along with the value of the computed Upper Control Limit.
#' @param scores output from `score_imp()` function.
#' @param alpha significance level used for computing the Upper Control Limit (UCL).
#' Defaults to 0.005.
#' @param phase 1 or 2. Defaults to 2.
#' @param m number of Phase I samples.
#'
#' @return `t2.chart` returns a list containing the following components:
#' \describe{
#'   \item{Plot}{ggplot object with \eqn{T^2} Control Chart of PCs}
#'   \item{poc}{Proportion of out-of-control samples}
#'   \item{out}{ID of out-of-control samples}
#'   \item{UCL}{Value of the Upper Control Limit}
#' }
#'
#' @details The computation of the Upper Control Limit (UCL)
#' depends on the `phase` argument. The chart is implemented for samples
#' of size 1.
#' @references MacGregor, J. F., & Kourti, T. (1995).
#' "Statistical process control of multivariate processes."
#' Control engineering practice, 3(3), 403-414.
#'
#' Wold, S., Geladi, P., Esbensen, K., & Ohman, J. (1987).
#' "Multi‐way principal components‐and PLS‐analysis"
#' Journal of chemometrics, 1(1), 41-56.
#'
#' @export
#' @seealso [score_imp()]
#' @examples
#' #Score imputation with CMR method
#' scores <- score_imp(
#'   data1 = fruits1,
#'   data2 = fruits2na,
#'   A = 4,
#'   method = "CMR"
#'   )
#' #Phase I data has 36 observatios
#' t2.chart(scores = scores, alpha = 0.005, phase = 2, m = 36)
t2.chart <- function(scores, alpha = 0.005, phase = 2, m) {

  x <- scores$Scores
  n <- nrow(x)
  A <- ncol(x)

  #UPPER CONTROL LIMIT
  if (phase == 1) {
    UCL <- stats::qbeta(1 - alpha, A/2, (n-A-1)/2)*(n-1)^2/n
  } else if (phase == 2) {
    UCL <- stats::qf(1 - alpha, df1 = A, df2 = m-A) * A * (m+1) * (m-1) / (m * (m-A))
  } else {
    stop("phase argument can take values 1 or 2")
  }

  titahat <- diag(scores$VarScores[1:A])
  tauC <- x

  salida <- NULL

  for (j in 1:nrow(tauC)) {
    filaj <- tauC[j, ]
    estj <- filaj %*% solve(titahat) %*% matrix(filaj, ncol = 1) #T2 para cada fila
    salida <- c(salida, estj)
  }

  y <- data.frame(orden = 1:n, t2 = salida, control = salida < UCL)
  data_texto <- data.frame(x = 1, y = UCL*1.05, label = "UCL")

  graf <- ggplot2::ggplot(data = y) +
    ggplot2::aes(x = orden, y = t2) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(ggplot2::aes(col = control), size = 2) +
    ggplot2::geom_hline(yintercept = UCL, col = "blue", linewidth = 1.2, linetype = "twodash") +
    ggplot2::geom_text(data = data_texto, ggplot2::aes(x = x, y = y, label = label), fontface = "bold") +
    ggplot2::scale_color_manual(breaks = c(FALSE, TRUE),
                                values = c("red", "black")) +
    ggplot2::ggtitle(expression(paste("T"^"2", " Control Chart"))) +
    ggplot2::xlab("Order") +
    ggplot2::ylab(expression(paste("T"^"2"))) +
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
