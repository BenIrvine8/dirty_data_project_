library(tidyverse)
library(janitor)

library(readxl)

candy_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx") %>%
  clean_names()
candy_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx") %>%
  clean_names()
candy_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx") %>%
  clean_names()

candy_2015_clean <- candy_2015 %>%
  mutate(person_id = row_number()) %>%
  pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
  mutate(year = 2015, gender = NA, country = NA) %>%
  select(year, person_id, going_out = are_you_going_actually_going_trick_or_treating_yourself,
    age = how_old_are_you, gender, country, candy, rating) 


candy_2016_clean <- candy_2016 %>%
  mutate(person_id = max(candy_2015_clean$person_id) + row_number()) %>%
  pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
  mutate(year = 2016) %>%
  select(year, person_id, going_out = are_you_going_actually_going_trick_or_treating_yourself,
         age = how_old_are_you, gender = your_gender, country = which_country_do_you_live_in,
         candy, rating) 

names(candy_2017) <- str_remove(names(candy_2017), "q[0-9]+_")
candy_2017_clean <- candy_2017 %>%
  rename(x100_grand_bar = `100_grand_bar`) %>%
  mutate(person_id = max(candy_2016_clean$person_id) + row_number()) %>%
  pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
  mutate(year = 2017)  %>%
  select(year, person_id, going_out, age, gender, country, candy, rating)



candy <- rbind(candy_2015_clean, candy_2016_clean, candy_2017_clean)

candy %>%
  distinct(country)

uk <- c("uk", "scotland", "england", "ireland", "u.k.", "united kindom")

us <- c("unites states", "murica", "united state", "united stated", "united ststes",
        "trumpistan", "united sates", "merica", "'merica", "ahem....amerca", "alaska",
        "murrika", "california", "new jersey", "new york", "north carolina", "pittsburgh",
        "u s", "unhinged states", "unied states", "unite states", "united staes", "united statea", "united statss",
        "the yoo ess of aaayyyyyy", "united stetes",  "units states",  "cascadia", "the republic of cascadia")

unk <- c("a", "atlantis", "canae", "earth", "endland", "europe", "fear and loathing",
         "i don't know anymore", "insanity lately", "narnia", "soviet canuckistan", "ud",
         "a tropical island south of the equator", "denial", "eua", "god's country", "neverland", "one of the best ones",
         "see above", "somewhere", "there isn't one for old men", "this one")


candy_clean_countries <- candy %>%
  mutate(country = if_else(country %in% c("canada`", "can"), "canada", country),
    country = if_else(country == "españa", "spain", country),
    country = if_else(country == "korea", "south korea", country),
    country = if_else(country == "the netherlands", "netherlands", country),
    country = if_else(country %in% uk, "united kingdom", country),
    country = if_else(str_detect(country, "usa"), "united states", country),
    country = if_else(str_detect(country, "us"), "united states", country),
    country = if_else(str_detect(country, "united states"), "united states", country),
    country = if_else(str_detect(country, "u.s."), "united states", country),
    country = if_else(str_detect(country, "america"), "united states", country),
    country = if_else(country %in% us, "united states", country),
    country = if_else(str_detect(country, "[0-9]"), NA_character_, country), 
    country = if_else(country %in% unk, NA_character_, country))

candy_clean_valid <- candy_clean_countries %>% 
  mutate(rating = str_to_lower(rating), country = str_to_lower(country),
         age = str_extract(age, "\\d+") %>% as.numeric(age))

candy_clean_valid_age <- candy_clean_valid %>%
  mutate(age = if_else(age > 120, NA_real_, age))

write_csv(candy_clean_valid_age, "clean_data/candy_clean.csv")
