#' GFT - Generalized F test
#'
#' \code{GFT} runs a generalized F test on linear combinations of regression outputs
#' @param R A matrix of selected terms in restricitons
#' @param r A vector of restrictions such that matrix multiplication of R and BetaHat equals r
#' @param V The variance-covariance matrix produced by the regression object
#' @param BetaHat a vector of estimated coefficients from the model
#'
#' @return Ftest the F-statistic for the performed test
#' @export
#'
#' @examples
#' data(iris)
#' reg <- lm(Sepal.Length ~ Sepal.Width + Petal.Length)
#'
GFT <- function(R,r,V,BetaHat)
{

  FTest<- ( t(R %*% BetaHat - r) %*%
              ginv( R %*% V %*% t(R))
            %*% (R %*% BetaHat-r)) /length(r)

  return(FTest)

}