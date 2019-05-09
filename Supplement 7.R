# Chuliang Song, György Barabas, Serguei Saavedra. 
# "On the consequences of the interdependence of stabilizing and equalizing mechanisms"
# The American Naturalist, 2019.

# This R code repoduces Supplement 7

library(tidyverse)
library(ggalt)

# compute niche overlap and fitness difference in MacArthur's model
para <- function(mu1, mu2, sigma = 1, omega = .5) {
  rho <- exp(-(mu1 - mu2)^2 / (4 * sigma^2))
  kappa <- exp((mu2^2 - mu1^2) / (2 * sigma^2 + 2 * omega^2))

  c(rho, kappa)
}

reciprocal <- function(x)  1 / x

# generate correlated mu1 and mu2 and then compute the correlation between fitness and niche overlap
correlation_plot <- function(correlation) {
  # generate mu1 and mu2 from multivariate normal distribution
  data_ori <-
    MASS::mvrnorm(
      n = 100000, #number of samples
      mu = c(0, 0), #both have mean 0
      Sigma = matrix(c(1, correlation, correlation, 1), ncol = 2), #correlation matrix in multivariate normal distribution
      tol = 1e-6, empirical = FALSE, EISPACK = FALSE
    ) %>%
    as.tibble() %>%
    mutate(group = rep(1:(n() / 10), 10))

  data_tranform <- data_ori %>%
    {
      map2(
        .$V1,
        .$V2,
        ~ para(.x, .y, sigma = 1, omega = .5)
      ) #compute the corresponding niche overlap and fitness
    } %>% 
    as.data.frame() %>%
    t() %>%
    as.tibble() %>%
    mutate(group = rep(1:(n() / 10), 10)) #divide them into groups where each group has ten samples
  
  bind_cols(
    data_ori %>%
      split(.$group) %>%
      map_dbl(~ cor(.$V1, .$V2)) %>%
      as.tibble() %>%
      rename("Mechanistic" = value),

    data_tranform %>%
      split(.$group) %>%
      map_dbl(~ cor(.$V1, .$V2)) %>%
      as.tibble() %>%
      rename("Phenomenological (MCT)" = value)
  ) %>%
    gather(type, value) %>%
    mutate(correlation = correlation)
}

data <- bind_rows(
  correlation_plot(0.9),
  correlation_plot(0),
  correlation_plot(-0.9)
)


# generate plot
data %>%
  mutate(correlation = case_when(
    correlation > 0 ~ "Positive",
    correlation == 0 ~ "Null",
    correlation < 0 ~ "Negative"
  )) %>%
  ggplot(aes(x = value)) +
  facet_grid(type ~ correlation, scales = "free_y") +
  stat_bkde(alpha = 1 / 2) +
  labs(
    x = "Correlation",
    y = "Density"
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    panel.spacing = unit(2, "lines"),
    text = element_text(size=20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black")
  )
