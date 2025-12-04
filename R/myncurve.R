#' My normal curve
#'
#' @param mu a vector of means
#' @param sigma a vector of standard deviations
#' @param a discrete random variable
#' @importFrom graphics polygon
#' @importFrom stats dnorm
#'
#' @returns a normal curve of probability
#' @export
#'
#' @examples myncurve(0,1, 0)
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xnorm = seq(mu-3*sigma, a, length = 1000)
  ynorm = dnorm(xnorm, mean = mu, sd = sigma)
  polygon(c(mu-3*sigma, xnorm, a), c(0, ynorm, 0), col = "pink")
  prob = pnorm(a, mean = mu, sd = sigma)
  list(mu = mu, sigma = sigma, a = a, prob = prob)
}
