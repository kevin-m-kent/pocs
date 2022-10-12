library(tidyverse)

raw_joyce <- read_delim(here::here("Week_6", "Data", "ulysses.txt"),
                        delim = ": ", col_names = c("word", "word_freq")
                        )

rho_data <- raw_joyce |>
  summarise(distinct_words = n_distinct(word), total_times = sum(word_freq)) |>
  mutate(innovation_rate = distinct_words/total_times) |>
  pull(innovation_rate) |>
  pluck(1)

get_theo_nk <- function(k, rho) {

  switch(k,
         "one" = 1/(2-rho),
         "two" = (1-rho)/((3-2*rho)*(2-rho)),
         "three" =(2*(1-rho)^2)/((4-3*rho)*(3-2*rho)*(2-rho))
  )

}

prop_theo <- map_dbl(c("one", "two", "three"), get_theo_nk, rho_data)

raw_joyce |>
  count(word_freq) |>
  arrange(word_freq) |>
  mutate(total_n = sum(n)) |>
  mutate(prop_calc = n/total_n) |>
  filter(word_freq <= 3) |>
  mutate(prop_theo = prop_theo) |>
  rename(rho_data = prop_calc, rho_theo = prop_theo) |>
  select(word_freq, rho_data, rho_theo) |>
  xtable()

# Pride and Prejudice, Monte Cristo --------------------------------------------

library(tidytext)
library(janeaustenr)
library(xtable)

count_freq <- map(c("pride_p..txt", "monte_cristo.txt"), ~
                  read_file(here::here("Week_6", "Data", .))) |>
  map(~ as.data.frame(.) |> rename(text = 1) |> unnest_tokens(word, text)) |>
  map(~ . |> count(word, name = "word_count")|> count(word_count) |>
        mutate(total_count = sum(n)) |>
        mutate(n = n/total_count) |>
        filter(word_count %in% 1:3)
  )

files <- c("pride_p..txt", "monte_cristo.txt")
names(files) <-   c("Pride and Prejudice", "Le comte de Monte-Cristo")

books_raw_token <- map(files, ~
                        read_file(here::here("Week_6", "Data", .))) |>
  map_dfr(~ as.data.frame(.) |> rename(text = 1) |> unnest_tokens(word, text), .id = "book")

count_english <- list("one", "two", "three")


observed_props <- books_raw_token |>
  count(word, book, name = "word_count") |>
  group_by(book) |>
  count(word_count) |>
  mutate(total_times = sum(n))  |>
  mutate(prop_observed = n/total_times) |>
  filter(word_count %in% 1:3) |>
  mutate(word_count = map(word_count, ~ count_english[.])) |>
  mutate(word_count = unlist(word_count)) |>
  select(book, word_count, prop_observed)

rhos <- map(c("pride_p..txt", "monte_cristo.txt"), ~
             read_file(here::here("Week_6", "Data", .))) |>
  map(~ as.data.frame(.) |> rename(text = 1) |> unnest_tokens(word, text)) |>
  map(~ . |> count(word, name = "word_count") |>
        summarise(distinct_words = n_distinct(word), total_times = sum(word_count)) |>
        mutate(innovation_rate = distinct_words/total_times) |>
        pull(innovation_rate) |>
        pluck(1))

names(rhos) <-  c("Pride and Prejudice", "Le comte de Monte-Cristo")
rhos <- enframe(rhos, name = "book", value = "rho") |>
  mutate(rho = unlist(rho))

names(raw_data) <- c("Pride and Prejudice", "Le comte de Monte-Cristo")

word_freqs <- bind_rows(raw_data, .id = "book")

combos <- crossing(word_count = c("one", "two", "three"),
                   rho = rhos$rho) |>
  rowwise() |>
  mutate(expected = list(get_theo_nk(word_count, rho))) |>
  left_join(rhos) |>
  ungroup() |>
  mutate(expected = unlist(expected)) |>
  left_join(observed_props) |>
  select(book, word_count, expected, prop_observed)

combos |>
  mutate(num = map(word_count, ~ which(count_english == .))) |>
  mutate(num = unlist(num)) |>
  arrange(book, num) |>
  relocate(word_count) |>
  select(-num) |>
  xtable()
