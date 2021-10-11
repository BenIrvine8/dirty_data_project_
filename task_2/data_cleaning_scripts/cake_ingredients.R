library(tidyverse)
library(janitor)
names <- read_csv("raw_data/cake_ingredient_code.csv")
cake <- read_csv("raw_data/cake-ingredients-1961.csv")

cake_clean <-
  cake %>%
  pivot_longer(-Cake, names_to = "code", values_to = "amount") %>%
  rename(cake = Cake) %>%
  left_join(names, by = "code") %>%
  drop_na(amount) %>%
  select(-code)

write_csv(cake_clean, "clean_data/cake_clean.csv")
