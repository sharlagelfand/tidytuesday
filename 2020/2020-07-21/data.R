library(tidytuesdayR)
rspca_tt <- tt_load("2020-07-21")
animal_outcomes <- rspca_tt[["animal_outcomes"]]
saveRDS(animal_outcomes, here::here("2020", "2020-07-21", "animal_outcomes.rds"))
