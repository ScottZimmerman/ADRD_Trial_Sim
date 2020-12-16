#' Plots the estimated effect size at different end points
#'
#' @param inputs list. Contains `months` character vector. List of study end points
#'                              `effect_size` character vector. List of estimated effect sizes at each end point
#'                              `ci_low` character vector. List of low end of confidence interval for each effect estimate
#'                              `ci_high` character vector. List of high end of confidence interval for each effect estimate
#'
#' @return none. Plots the baseline distribution of a given variable
library("ggplot2")
library("tidyr")
library("dplyr")
effect_plot <- function(inputs){
  months <- inputs[["months"]]
  effect_size <- inputs[["effect_size"]]
  ci_low <- inputs[["ci_low"]]
  ci_high <- inputs[["ci_high"]]

  df <- as.data.frame(cbind(months, effect_size, ci_low, ci_high))
  df <- df %>% mutate(months = months)

  df %>%
    ggplot() +
    geom_hline(aes(yintercept = 0), color = "grey50", lty = "dashed") +
    geom_point(aes(y = effect_size, x = months), size = 3) +
    geom_segment(aes(y = ci_low, yend = ci_high, x = months, xend = months), size = 1) +
    theme_minimal() +
    theme(axis.line = element_line(colour = "grey50", size = 1),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none") +
    scale_x_continuous(breaks=months,limits = c(0,max(months)+20)) +
    labs(title = "Estimated Effect Size of Intensive Treatment, Measured at Different Times") +
    ylab("Estimated Effect Size (Bars show 95% CI)") + xlab("Time (months)")
}
#
#
# months <- c(40,80,120,160,200,240)
# effect_size <- c(-0.041, -.062, -.11,-0.23,-0.31,-0.42)
# ci_low <- c(-0.428, -0.505, -.522,-0.61,-0.74,-0.81)
# ci_high <- c(0.324, 0.288, .201,.141,.045,-0.026)
#
# inputs <- list("months" = months,
#                "effect_size" = effect_size,
#                "ci_low" = ci_low,
#                "ci_high" = ci_high)
# effect_plot(inputs)
# ggsave("aeffect_plot.tiff")
