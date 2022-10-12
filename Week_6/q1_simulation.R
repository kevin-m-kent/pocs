library(furrr)
library(tidyverse)
plan(multisession, workers = 3)

k <- 1:10000 ## range of k
c <- 1
gamma <- .5
pmfs <- c*k^(-gamma)

n_samples <- 1:1000
sample_size <- 50
maxes <- c()

N_size <- 1000

get_kminmax <- function(n_samples, sample_size) {

  for (i in 1:n_samples) {

    max_k <- sample(k, size = sample_size, prob = pmfs) |>
      max()

    maxes <- c(maxes, max_k)

  }

  min(maxes)


}

mins_observed <- future_map(n_samples, get_kminmax, sample_size = sample_size) |>
  unlist()

plot(n_samples, mins_observed)

sim_data <- tibble(N_size = n_samples, obs = mins_observed)

lm(log10(obs) ~ log10(N_size), data = sim_data)

derived_func <- function(n, gamma, c) {

  ((-gamma + 1)/(c*n))^(1/(-gamma +1))

}

derived_ks <- map(N_size, derived_func, gamma = gamma, c = c) |>
  unlist()

plot(N_size, derived_ks)

lm()
