asl_bootstrap_one_sample <- function(y, mu0, j = 3000, alpha_level = 0.05,
                                     alternative = c("two.sided", "less", "greater")) {

  if (length(y) < 2) stop("Sample size must be at least 2.")
  alternative <- match.arg(alternative)
  n <- length(y)

  # 1. Calculate observed statistics (tc)
  y_bar <- mean(y)
  s <- sd(y)
  tc <- (y_bar - mu0) / (s / sqrt(n))

  # 2. Transform data to satisfy H0
  x_null <- y - y_bar + mu0

  # 3. Resample and calculate bootstrap test statistics (tj)
  tj_values <- numeric(j)

  for (i in 1:j) {
    bootstrap_sample <- sample(x_null, size = n, replace = TRUE)
    s_j <- sd(bootstrap_sample)
    # If standard deviation is 0, skip to avoid division by zero (should be rare)
    if (s_j > 0) {
      x_bar_j <- mean(bootstrap_sample)
      tj_values[i] <- (x_bar_j - mu0) / (s_j / sqrt(n))
    } else {
      tj_values[i] <- 0
    }
  }

  # 4. Calculate ASL (p-value estimate)
  if (alternative == "greater") {
    ASL <- sum(tj_values >= tc) / j
    critical_t <- quantile(tj_values, 1 - alpha_level)
    rejection_area <- data.frame(t = tj_values[tj_values >= critical_t])
  } else if (alternative == "less") {
    ASL <- sum(tj_values <= tc) / j
    critical_t <- quantile(tj_values, alpha_level)
    rejection_area <- data.frame(t = tj_values[tj_values <= critical_t])
  } else { # two.sided
    ASL <- (sum(tj_values >= abs(tc)) + sum(tj_values <= -abs(tc))) / j
    lower_crit <- quantile(tj_values, alpha_level / 2)
    upper_crit <- quantile(tj_values, 1 - alpha_level / 2)

    # Define rejection area for visualization
    rejection_area <- data.frame(t = tj_values[tj_values <= lower_crit | tj_values >= upper_crit])
  }

  # 5. Generate Monte Carlo Quality Plot (Stabilization of ASL)
  if (alternative == "two.sided") {
    # For two-sided, use absolute value for extremeness check
    extreme_indicator <- abs(tj_values) >= abs(tc)
  } else if (alternative == "greater") {
    extreme_indicator <- tj_values >= tc
  } else {
    extreme_indicator <- tj_values <= tc
  }

  cumulative_asl <- cumsum(extreme_indicator) / 1:j

  mc_plot <- ggplot(data.frame(Iteration = 1:j, Cumulative_ASL = cumulative_asl),
                    aes(x = Iteration, y = Cumulative_ASL)) +
    geom_line(color = "darkgreen") +
    labs(title = paste("Monte Carlo Quality Check: ASL Convergence (J=", j, ")"),
         y = "Cumulative ASL Estimate", x = "Number of Iterations") +
    theme_minimal()

  # 6. Generate Density Plot
  density_plot <- ggplot(data.frame(t = tj_values), aes(x = t)) +
    geom_density(fill = "gray80", alpha = 0.6) +
    # Highlight Rejection Region (based on bootstrap quantiles)
    geom_vline(xintercept = if (alternative == "two.sided") c(lower_crit, upper_crit) else critical_t,
               linetype = "dashed", color = "red") +
    # Highlight Observed T Statistic
    geom_vline(xintercept = tc, color = "blue", size = 1, linetype = "solid") +
    annotate("text", x = tc, y = max(density(tj_values)$y) * 0.9,
             label = paste("Observed T =", round(tc, 2)), color = "blue", hjust = ifelse(tc > mean(tj_values), 1.1, -0.1)) +
    labs(title = paste("Empirical Sampling Distribution of T under H0"),
         subtitle = paste("ASL =", round(ASL, 4), "| Rejection limits based on α =", alpha_level),
         x = "Bootstrap T Statistic (tj)", y = "Density") +
    theme_minimal()

  return(
    list(
      H0 = paste("mu =", mu0),
      Ha = paste("mu", ifelse(alternative == "two.sided", "!=",
                              ifelse(alternative == "greater", ">", "<")), mu0),
      observed_t = tc,
      bootstrap_ASL = ASL,
      alternative = alternative,
      mc_plot = mc_plot,
      density_plot = density_plot
    )
  )
}



