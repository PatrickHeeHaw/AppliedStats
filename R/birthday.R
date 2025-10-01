#' Calculate Brithday Probability
#'
#' @param n Integer or vecotr of itnegers
#'
#' @returns vector of probabilities
#' @export
#'
#' @examples
#' #Probbability for a class of 23 people
#' birthday(23)
#'
birthday <- function(n){

  log_prob_different<- lchoose(365, n) + lfactorial(n) - n * log(365)

prob_shared <- 1 - exp(log_prob_different)

return(prob_shared)

}
