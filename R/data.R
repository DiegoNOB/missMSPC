#' Aminoacid concentration in fruit juice samples
#'
#' Simulated data set of Phase I samples of pure fruit juice.
#' Original concentrations are assumed to be measured in \eqn{\mu}g per standard volume.
#' Variables are standardized.
#'
#' @name fruits1
#' @format A data frame with 36 rows and 11 variables:
#'
#' \describe{
#'
#'   \item{LYS}{lysine}
#'   \item{ARG}{arginine}
#'   \item{ASP}{aspartic acid}
#'   \item{SER}{serine}
#'   \item{GLU}{glutamine acid}
#'   \item{PRO}{proline}
#'   \item{GLY}{glycine}
#'   \item{ALA}{alanine}
#'   \item{VAL}{valine}
#'   \item{PHA}{phenyl alanine}
#'   \item{GABA}{gamma-amino butric acid}
#' }
#'
#' @seealso [fruits2()], [fruits2na()]
#' @references Fuchs, S. & Kenett, R. S. (1998).
#' "Multivariate quality control: theory and applications".
#' Chapman and Hall/CRC.
"fruits1"

#' Aminoacid concentration in fruit juice samples
#'
#' Simulated data set of Phase II samples of pure fruit juice.
#' Original concentrations are assumed to be measured in \eqn{\mu}g per standard volume.
#' Variables are standardized.
#'
#' @name fruits2
#' @format A data frame with 30 rows and 11 variables:
#'
#' \describe{
#'
#'   \item{LYS}{lysine}
#'   \item{ARG}{arginine}
#'   \item{ASP}{aspartic acid}
#'   \item{SER}{serine}
#'   \item{GLU}{glutamine acid}
#'   \item{PRO}{proline}
#'   \item{GLY}{glycine}
#'   \item{ALA}{alanine}
#'   \item{VAL}{valine}
#'   \item{PHA}{phenyl alanine}
#'   \item{GABA}{gamma-amino butric acid}
#' }
#'
#' @seealso [fruits1()], [fruits2na()]
#' @references Fuchs, S. & Kenett, R. S. (1998).
#' "Multivariate quality control: theory and applications".
#' Chapman and Hall/CRC.
"fruits2"

#' Aminoacid concentration in fruit juice samples
#'
#' Simulated data set of Phase II samples of pure fruit juice.
#' Original concentrations are assumed to be measured in \eqn{\mu}g per standard volume.
#' Variables are standardized.
#' Missing values were generated using the `ampute()` function from package `mice`,
#' assuming a MCAR missingness mechanism.
#'
#' @name fruits2na
#' @format A data frame with 30 rows and 11 variables:
#'
#' \describe{
#'
#'   \item{LYS}{lysine}
#'   \item{ARG}{arginine}
#'   \item{ASP}{aspartic acid}
#'   \item{SER}{serine}
#'   \item{GLU}{glutamine acid}
#'   \item{PRO}{proline}
#'   \item{GLY}{glycine}
#'   \item{ALA}{alanine}
#'   \item{VAL}{valine}
#'   \item{PHA}{phenyl alanine}
#'   \item{GABA}{gamma-amino butric acid}
#' }
#'
#' @seealso [fruits1()], [fruits2()]
#' @references Fuchs, S. & Kenett, R. S. (1998).
#' "Multivariate quality control: theory and applications".
#' Chapman and Hall/CRC.
"fruits2na"
