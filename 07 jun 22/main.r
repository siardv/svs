
# install.packages("haven")
# install.packages("tidyverse"")
# install.packages("magrittr")
# install.packages("readxl")
# install.packages("xlsx")

library(haven)
library(tidyverse)
library(magrittr)
library(readxl)
library(xlsx)

fix_names <- function(.df) {
  .df %>% set_names(names(.) %>%
    tolower() %>%
    map_chr(~ str_replace_all(
      .,
      c("(\\(.*\\))" = "", "[.]+" = " ", "\\?|\\;" = "", "[+]" = "_en_")
    ) %>%
      {
        gsub("\\s", "_", str_squish(.))
      }))
}

xlsx_to_df <- function(.file) {
  paste0(getwd(), "/", .file) %>%
    read_excel(col_names = TRUE, trim_ws = TRUE) %>%
    fix_names()
}

dataset <-
  xlsx_to_df(paste0(getwd(), "/", "Data experiment.xlsx"))

dataset %>%
  mutate(across(
    matches("antwoord_"),
    ~ str_match(.x, "[.0-9]+") %>% as.numeric()
  ))

dataset$participant_a2_1 <-
  select(dataset, antwoord_flashes) %>%
  unlist() %>%
  map_lgl(~ grepl("1", .)) %>%
  ifelse(1, NA)
dataset$participant_a2_2 <-
  select(dataset, antwoord_flashes) %>%
  unlist() %>%
  map_lgl(~ grepl("2", .)) %>%
  ifelse(2, NA)

dataset$participant_a2 <-
  map2(
    dataset$participant_a2_1,
    dataset$participant_a2_2, ~ ifelse(is.na(.x), .y, .x)
  ) %>% unlist() # %>% as_tibble_col()

dataset$participant_b2_1 <-
  select(dataset, antwoord_flashes) %>%
  unlist() %>%
  map_lgl(~ grepl("7", .)) %>%
  ifelse(7, NA)
dataset$participant_b2_2 <-
  select(dataset, antwoord_flashes) %>%
  unlist() %>%
  map_lgl(~ grepl("8", .)) %>%
  ifelse(8, NA)

dataset$participant_b2 <-
  map2(
    dataset$participant_b2_1,
    dataset$participant_b2_2, ~ ifelse(is.na(.x), .y, .x)
  ) %>% unlist()

more_than_2 <-
  dataset %>%
  select(antwoord_flashes) %>%
  unlist() %>%
  str_split("") %>%
  map(~ keep(., grepl("1|2|7|8", .))) %>%
  {
    which(lengths(.) > 2)
  }

reactietijd_index <-
  dataset %>%
  select(antwoord_flashes) %>%
  unlist() %>%
  str_split("") %>%
  map(~ keep(., grepl("1|2|7|8", .))) %>%
  map_if(~ `==`(length(.), 2), ~.x, .else = 0) %>%
  # keep(~is.null(.) %>% not()) %>%
  map(~ as.numeric(.) %>%
    {
      c(which.min(.), which.max(.))
    })

reactietijd_values <-
  dataset %>%
  select(antwoord_flash_reactietijd) %>%
  unlist() %>%
  {
    gsub("\\]|\\[|[ ]+", "", .)
  } %>%
  str_split(",", 2) %>%
  map(~ as.character(.) %>%
    as.numeric())

ls_temp <-
  list(reactietijd_index, reactietijd_values) %>%
  transpose() %>%
  map(~ .[[2]][sort(.[[1]], index.return = TRUE)[[2]]])

dataset$antwoord_flash_reactietijd_a1 <-
  dataset$antwoord_flash_reactietijd_a2 <-
  dataset$antwoord_flash_reactietijd_b1 <-
  dataset$antwoord_flash_reactietijd_b2 <-
  NA_real_

for (i in 1:length(ls_temp)) {
  cat("row: ", i, "\r")
  if (length(ls_temp[[i]]) == 2) {
    dataset$antwoord_flash_reactietijd_a1[[i]] <- ls_temp[[i]][[1]]
    dataset$antwoord_flash_reactietijd_b1[[i]] <- ls_temp[[i]][[2]]
  }
}

sc <-
  dataset$antwoord_flash_reactietijd %>%
  map(~ str_match_all(., ".") %>%
    length(.)) %>%
  unlist()

afrt <-
  dataset$antwoord_flash_reactietijd %>%
  {
    gsub("\\[([0-9]+.[0-9]+)\\]", "\\1", .)
  } %>%
  map_dbl(~ as.numeric(.))

for (i in 1:length(sc)) {
  cat("row: ", i, "\r")
  if (!is.na(sc[i])) {
    if (sc[i] == 1) {
      if (any(dataset$participant_a[i] %in% 1:2)) {
        dataset$antwoord_flash_reactietijd_a2[i] <- afrt[i]
      } else if (any(dataset$participant_b[i] %in% 7:8)) {
        dataset$antwoord_flash_reactietijd_b2[i] <- afrt[i]
      }
    }
  }
}

dataset$antwoord_flash_reactietijd_a <-
  map2(
    dataset$antwoord_flash_reactietijd_a1,
    dataset$antwoord_flash_reactietijd_a2, ~ ifelse(is.na(.x), .y, .x)
  ) %>% unlist()

dataset$antwoord_flash_reactietijd_b <-
  map2(
    dataset$antwoord_flash_reactietijd_b1,
    dataset$antwoord_flash_reactietijd_b2, ~ ifelse(is.na(.x), .y, .x)
  ) %>% unlist()

dataset %<>%
  select(!c(
    participant_a2_1,
    participant_a2_2,
    participant_b2_1,
    participant_b2_2,
    antwoord_flash_reactietijd_a1,
    antwoord_flash_reactietijd_a2,
    antwoord_flash_reactietijd_b1,
    antwoord_flash_reactietijd_b2
  ))

haven::write_sav(dataset, paste0("df_", Sys.Date(), ".sav"))
xlsx::write.xlsx(dataset, paste0("df_", Sys.Date(), ".xlsx"))
