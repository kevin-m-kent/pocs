library(tidyverse)
library(httr)

raw_data <- read_csv(here::here("Week_1", "hw_1_data.csv")) |>
  select(-9)

clean_df <- function(raw_df) {

  clean_df <- raw_df |>
    mutate(class = case_when(str_detect(Event, "kg") ~ Event, TRUE ~ NA_character_)) |>
    fill(class, .direction = "down") |>
    drop_na() |>
    mutate(Record = str_extract(Record, "[0-9]+") |> as.numeric(),
           class = str_extract(class, "[0-9]+") |> as.numeric())

  clean_df

}

cleaned_data <- raw_data |> clean_df()

cleaned_data |>
  ggplot(aes(class, Record, col = Type)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(Event ~ ., scales = "free")

nested_mods <- cleaned_data |>
  mutate(Record = str_extract(Record, "[0-9]+") |> as.numeric(),
         class = str_extract(class, "[0-9]+") |> as.numeric()) |>
  group_nest(Type, Event) |>
  mutate(mod = map(data, ~ lm(log10(Record) ~ log10(class), data = .) |> broom::tidy())) |>
  select(-data) |>
  unnest(mod)

coeffs <- nested_mods |>
  select(Type:estimate) |>
  pivot_wider(names_from = "term", values_from = "estimate") |>
  rename(c = 3, beta = 4) |>
  mutate(c = 10^c)

cleaned_data |>
  left_join(coeffs) |>
  mutate(pred = c*class^beta) |>
  mutate(normalized_record = 100*(Record/((c*class^beta) - 1))) |>
  arrange(desc(normalized_record)) |>
  View()

