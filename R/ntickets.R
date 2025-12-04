#' ntickets
#'
#' @param N The number of seats in the flight
#' @param p The probability fo the passenger showing up to the flight
#' @param gamma probability a plan will be truly overbooked
#'
#' @returns a list contatin nd, nc, N, p, and gamma
#' @export
#'
#' @examples
ntickets <- function(N = 200, p = .95, gamma = .02) {

  # ------------ discrete distribution

  # possible amounts of tickets sold, where size = nd (sample size) and N is the amount of seats on the flight
  nd = seq(from = N, to = 1.1*N, by = 1)

  #y axis of probabilities
  probd = 1-gamma-pbinom(N, nd, p)

  # pbinom(N, nd, p) generates probability of showing up for lower tail end
  # 1-gamma is the probability of not overbooking
  # when pbinom() = 1-gamma or total number show ups = not overbooked, we have the best number of tickets sold

  # if probd<0 is underbooking, and probd>0 is overbooking, find value of probd=0
  bestd = nd[which.min(abs(probd))] #which.min(abs()) finds when probability ~ 0

  # create discrete graph
  plot(x = nd, y = probd,
       main = "Objective Vs n to find optimal tickets sold discrete",
       ylab = "Objective", xlab = "n",
       type = "b")
  abline(v = bestd, h=probd[which.min(abs(probd))], col = "Red")
  text(x = bestd, y = .95, paste0("nd = ",bestd))

  # ------------ continuous distribution

  # possible amounts of tickets sold, continuous, n = nc, Y = N, pretend by is very very small
  nc = seq(from = N, to = 1.1*N, by = .00001)

  # expression for probabilities to use in curve(), where mean = np, sd = sqrt(npq)
  probc = 1-gamma-pnorm(N+.5, mean = nc*p, sqrt(nc*p*(1-p)), lower.tail = TRUE)

  # find value of probc = 0 for best number of tickets sold
  cfunc = function(x) {abs(1-gamma-pnorm(N+.5, mean = x*p, sqrt(x*p*(1-p))))}
  optimizedc = optimize(cfunc, nc)
  bestc = optimizedc$minimum

  # create continuous graph, where x is reparameterized nc
  curve(1-gamma-pnorm(N+.5, mean = x*p, sqrt(x*p*(1-p))),
        from = N, to = 1.1*N,
        main = "Objective Vs n to find optimal tickets sold continous",
        ylab = "Objective", xlab = "n")
  abline(v = bestc, h=probc[which.min(abs(probc))], col = "Blue")
  text(x = bestc, y = .95, paste0("nc = ",bestc))

  # ------------ print list
  tickets.list = list(bestd, bestc, N, gamma, p)
  names(tickets.list) = c("nd", "nc", "N", "gamma", "p")
  tickets.list
}
