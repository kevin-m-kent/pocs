library(tidyverse)

rhos <- c(.20, 0.10, 0.01, 0.001)

run_rich_richer_sim <- function(n_iterations, rho) {

  items <- c(1)

  for (i in 1:n_iterations) {

    if (runif(1) < rho) {

      items <- c(items, max(items) + 1)

    }

    else {

      add_this <- sample(items, 1)
      items <- c(items, add_this)

    }

  }

  items

}

rerun_simulation <- function(n_iterations, rho, n_repeats) {

  results <- rerun(n_repeats, run_rich_richer_sim(n_iterations, rho))

  results_df <- tibble(id = 1:n_repeats, results = results) |>
    mutate(results = map(results, unlist)) |>
    unnest(results)

  results_df |>
    mutate(rho = rho)

}

combinations <- crossing(n_iterations = c(10000), rho = rhos)

all_results <- map_dfr(rhos, ~ rerun_simulation(10000, ., 100))

all_results |>
  count(rho, results) |>
  ggplot(aes(n)) + geom_histogram() + scale_x_log10()

all_results |>
  count(rho, results, id) |>
  group_by(rho, results) |>
  summarise(n = mean(n)) |>
  group_by(rho) |>
  mutate(rank = rank(-n)) |>
  ungroup() |>
  mutate(rho = as.factor(rho)) |>
  ggplot(aes(log10(results), log10(n), col = rho)) + geom_point() + geom_smooth() +
  labs(title = "N_k by order of Entry", x = "Group by Entry Order", y = "Total Members",
       subtitle = "Averaged over 100 simulations of 1000 iterations") +
  theme_light()

ggsave(here::here("Week_6", "first_mover_2.png"))

all_results |>
  count(rho, results, id) |>
  group_by(rho, results) |>
  summarise(n = mean(n)) |>
  group_by(rho) |>
  mutate(rank = rank(-n)) |>
  mutate(rho = as.factor(rho)) |>
  ggplot(aes(log10(rank), log10(n), col = rho)) + geom_point() + geom_smooth(method = "lm") +
  theme_light() + #+ facet_wrap(rho ~ ., scales = "free") +
  labs(title = "Zipf Rank ~ Frequency", x = "Rank", y = "Frequency")

ggsave(here::here("Week_6", "zipf_sim.png"), width = 12, height = 8)


all_results |>
  count(rho, results, id, name = "raw_freq") |>
  group_by(rho, results) |>
  summarise(n = mean(raw_freq)) |>
  group_by(rho) |>
  mutate(rank = rank(-n)) |>
  group_by(rho) |>
  group_nest() |>
  rowwise() |>
  mutate(mods = list(lm(log10(n) ~ log10(rank), data = data) |> broom::tidy())) |>
  select(-data) |>
  unnest(mods) |>
  filter(str_detect(term, "log10")) |>
  mutate(estimate = abs(estimate)) |>
  rename(alpha = estimate) |>
  mutate(expected = 1 - rho) |>
  select(rho, alpha, expected) |>
  xtable()

all_results |>
  count(rho, results, id, name = "raw_freq") |>
  group_by(rho, id) |>
  mutate(distinct_groups = n_distinct(results)) |>
  group_by(rho) |>
  filter(raw_freq == 1) |>
  count(rho, distinct_groups, id) |>
  View()
