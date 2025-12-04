#' Calculate Birthday Probability
#'
#' @param n The class size of the fucntion
#'
#' @returns vector of probabilities
#' @export
#'
#' @examples
#' #Probability for a class of 23 people
#' birthday(23)
#'
birthday <- function(n){
  p <- lchoose(365, n) + lfactorial(n) - n * log(365)
ps <- 1 - exp(p)
return(ps)

}
