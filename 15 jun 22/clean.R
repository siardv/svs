
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

####### DEEL 1 #######

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

dataset <- xlsx_to_df("Data experiment.xlsx")

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

dataset$participant_b2_1 <- select(dataset, antwoord_flashes) %>%
  unlist() %>%
  map_lgl(~ grepl("7", .)) %>%
  ifelse(7, NA)
dataset$participant_b2_2 <- select(dataset, antwoord_flashes) %>%
  unlist() %>%
  map_lgl(~ grepl("8", .)) %>%
  ifelse(8, NA)

dataset$participant_b2 <-
  map2(
    dataset$participant_b2_1,
    dataset$participant_b2_2, ~ ifelse(is.na(.x), .y, .x)
  ) %>% unlist()

get_values <- function(.df, .colname) {
  .df[.colname] %>%
    unlist() %>%
    map(~ gsub("EN", ",", .) %>%
      {
        gsub("\\]|\\[|[ ]+|'", "", .)
      } %>%
      {
        ifelse(grepl(",", .),
          list(str_split(., ",")),
          list(.)
        ) %>%
          unlist()
      } %>%
      as.character() %>%
      as.numeric())
}

antwoord_flashes_values <- get_values(dataset, "antwoord_flashes")
antwoord_flash_reactietijd_values <- get_values(dataset, "antwoord_flash_reactietijd")

reactietijd <- map2(
  antwoord_flashes_values, antwoord_flash_reactietijd_values,
  ~ list(
    c(
      .x[which(`%in%`(.x, 1:2))][1],
      .x[which(`%in%`(.x, 7:8))][1]
    ) %>% .[!is.na(.)],
    c(
      .y[which(`%in%`(.x, 1:2))][1],
      .y[which(`%in%`(.x, 7:8))][1]
    ) %>% .[!is.na(.)]
  )
)

dataset$antwoord_flash_a <-
  dataset$antwoord_flash_b <-
  dataset$antwoord_flash_reactietijd_a <-
  dataset$antwoord_flash_reactietijd_b <- NA

for (i in 1:nrow(dataset)) {
  cat("row: ", i, "\r")
  if (reactietijd[[i]] %>% lengths() %>% sum() %>% equals(2)) {
    if (reactietijd[[i]][[1]] <= 2) {
      dataset$antwoord_flash_a[i] <- reactietijd[[i]][[1]][1]
      dataset$antwoord_flash_reactietijd_a[i] <- reactietijd[[i]][[2]][1]
    } else if (reactietijd[[i]][[1]] >= 7) {
      dataset$antwoord_flash_b[i] <- reactietijd[[i]][[1]][1]
      dataset$antwoord_flash_reactietijd_b[i] <- reactietijd[[i]][[2]][1]
    }
  } else if (reactietijd[[i]] %>% lengths() %>% sum() %>% equals(4)) {
    x <- reactietijd[[i]][[1]]
    dataset$antwoord_flash_a[i] <- x[which.min(x)]
    dataset$antwoord_flash_b[i] <- x[which.max(x)]

    dataset$antwoord_flash_reactietijd_a[i] <- reactietijd[[i]][[2]][which.min(x)]
    dataset$antwoord_flash_reactietijd_b[i] <- reactietijd[[i]][[2]][which.max(x)]
  }
}

dataset %<>%
  select(!c(
    participant_a2_1,
    participant_a2_2,
    participant_b2_1,
    participant_b2_2
  ))

####### DEEL 2 #######

antw_comm <-
  dataset %>%
  select(antwoord_communicatie) %>%
  unlist() %>%
  str_split("") %>%
  map(~ keep(., grepl("1|2|3|4|7|8|9|0", .)) %>%
    unique() %>%
    as.character() %>%
    paste0("") %>%
    na_if(`==`(nchar(.), 0)) %>%
    as.numeric())

dataset$antwoord_communicatie_a <-
  dataset$antwoord_communicatie_b <-
  dataset$antwoord_communicatie_reactietijd_a1 <-
  dataset$antwoord_communicatie_reactietijd_a2 <-
  dataset$antwoord_communicatie_reactietijd_b1 <-
  dataset$antwoord_communicatie_reactietijd_b2 <-
  NA

for (i in 1:nrow(dataset)) {
  if (any(!is.na(antw_comm[[i]]))) {
    if (lengths(antw_comm[i]) == 1) {
      if (antw_comm[[i]] %in% 1:4) {
        dataset$antwoord_communicatie_a[i] <- antw_comm[[i]]
      } else if (antw_comm[[i]] %in% c(0, 7:9)) {
        dataset$antwoord_communicatie_b[i] <- antw_comm[[i]]
      }
    } else if (lengths(antw_comm[i]) == 2) {
      a <- which(antw_comm[[i]] %in% 1:4)
      b <- which(antw_comm[[i]] %in% c(0, 7:9))
      dataset$antwoord_communicatie_a[i] <- antw_comm[[i]][a]
      dataset$antwoord_communicatie_b[i] <- antw_comm[[i]][b]
    }
  }
}


for (i in 1:nrow(dataset)) {
  cat("row: ", i, "\r")
  if (dataset[i, c("antwoord_communicatie_a", "antwoord_communicatie_b")] %>%
    unlist() %>% is.na() %>% which() %>% length() == 1) {
    if (dataset$antwoord_communicatie_a[i] %in% 1:4) {
      dataset$antwoord_communicatie_reactietijd_a1[i] <-
        dataset$antwoord_communicatie_en_reactietijd[i] %>%
        {
          gsub("\\]|\\[", "", .)
        } %>%
        as.character() %>%
        as.numeric()
    } else if (dataset$antwoord_communicatie_b[i] %in% c(0, 7:9)) {
      dataset$antwoord_communicatie_reactietijd_b1[i] <-
        dataset$antwoord_communicatie_en_reactietijd[i] %>%
        {
          gsub("\\]|\\[", "", .)
        } %>%
        as.character() %>%
        as.numeric()
    }
  } else if (dataset[i, c("antwoord_communicatie_a", "antwoord_communicatie_b")] %>%
    unlist() %>% is.na() %>% which() %>% length() == 0) {
    reactie_resp <-
      dataset$antwoord_communicatie[i] %>%
      str_extract_all("[0-9]", simplify = TRUE) %>%
      {
        gsub("0", "10", .)
      } %>%
      as.character() %>%
      as.numeric()

    reactie_tijd <-
      dataset$antwoord_communicatie_en_reactietijd[i] %>%
      unlist() %>%
      {
        gsub("\\]|\\[|[ ]+", "", .)
      } %>%
      str_split(",", 2) %>%
      map(~ as.character(.) %>%
        as.numeric()) %>%
      .[[1]] %>%
      .[sort(reactie_resp, index.return = TRUE)[[2]]] %>%
      as.numeric()

    dataset$antwoord_communicatie_reactietijd_a2[i] <- reactie_tijd[1]
    dataset$antwoord_communicatie_reactietijd_b2[i] <- reactie_tijd[2]
  }
}

dataset$antwoord_communicatie_reactietijd_a <-
  map2(
    dataset$antwoord_communicatie_reactietijd_a1,
    dataset$antwoord_communicatie_reactietijd_a2, ~ ifelse(is.na(.x), .y, .x)
  ) %>% unlist()

dataset$antwoord_communicatie_reactietijd_b <-
  map2(
    dataset$antwoord_communicatie_reactietijd_b1,
    dataset$antwoord_communicatie_reactietijd_b2, ~ ifelse(is.na(.x), .y, .x)
  ) %>% unlist()

dataset$participant_a <- dataset$participant_a2
dataset$participant_b <- dataset$participant_b2

dataset %<>% select(!c(
  participant_a2, participant_b2,
  antwoord_communicatie_reactietijd_a1,
  antwoord_communicatie_reactietijd_a2,
  antwoord_communicatie_reactietijd_b1,
  antwoord_communicatie_reactietijd_b2
))

dataset %<>% select(
  participant,
  sociale_condities,
  beep,
  flashes,
  communicatie,
  antwoord_flashes,
  participant_a,
  participant_b,
  antwoord_flash_reactietijd,
  antwoord_communicatie,
  antwoord_communicatie_en_reactietijd,
  antwoord_flash_a,
  antwoord_flash_b,
  antwoord_flash_reactietijd_a,
  antwoord_flash_reactietijd_b,
  antwoord_communicatie_a,
  antwoord_communicatie_b,
  antwoord_communicatie_reactietijd_a,
  antwoord_communicatie_reactietijd_b,
)

haven::write_sav(dataset, paste0("df_", Sys.Date(), ".sav"))
xlsx::write.xlsx(dataset, paste0("df_", Sys.Date(), ".xlsx"), showNA = FALSE)

####### DEEL 3 #######

dataset <-
  read_excel("~/Downloads/data gefiltrrd.xlsx", 1) %>%
  select(!1) %>%
  set_names(names(.) %>% tolower())

dataset %>%
  group_by(participant, sociale_condities, flashes, beep) %>%
  group_data() %>%
  list(dataset %>% group_by(participant, sociale_condities, flashes, beep) %>% group_map(~ {
    colMeans(.[c(
      "antwoord_flash_reactietijd_a",
      "antwoord_flash_reactietijd_b",
      "antwoord_communicatie_reactietijd_a",
      "antwoord_communicatie_reactietijd_b"
    )], na.rm = TRUE)
  }) %>% reduce(bind_rows)) %>%
  bind_cols() %>%
  add_column(select(., .rows) %>%
    map_df(~ lengths(.)) %>%
    set_names("n_rows"), .before = "participant") %>%
  select(!.rows) -> df

write.xlsx(df, "reactietijd_flash_comm_met__flashes_beeps.xlsx", showNA = FALSE)


dataset %<>%
  mutate(antwoord_flash_b = antwoord_flash_b - 6)

# deze filter is niet permanent en heeft geen effect op de rest
dataset %>%
  filter(
    antwoord_flash_reactietijd_a <= 3.6,
    antwoord_flash_reactietijd_b <= 3.6
  )

dataset %>%
  group_by(participant, sociale_condities, flashes, beep) %>%
  group_data() %>%
  list(dataset %>%
    group_by(participant, sociale_condities, flashes, beep) %>%
    group_map(~ {
      colMeans(.[c(
        "antwoord_flash_a",
        "antwoord_flash_b"
      )], na.rm = TRUE)
    }) %>%
    reduce(bind_rows)) %>%
  bind_cols() %>%
  add_column(select(., .rows) %>%
    map_df(~ lengths(.)) %>%
    set_names("n_rows"), .before = "participant") %>%
  select(!.rows) -> df

write.xlsx(df, "antwoord_flash_a_b.xlsx", showNA = FALSE)
haven::write_sav(dataset, "dataset.sav")

# Bovenstaande code berekent de gemiddelde reactietijd op flashes, per sociale conditie, flash en beep.

# Interpreatie van de output, rij 1:
# De eerste rij geeft het gemiddelde aan van de reactietijd  van deelnemer 1 in de sociale conditie 1, waarbij er 1 flash en 1 beep was
# Er zijn in totaal 60 rijen gebruikt om dit gemiddelde te berekenen (dit is inclusief missende waarden; 60 betekent niet 60 valide waarden).
# 1.61 betekent dat deelnemer 1a in 1.61 seconden heeft gereageerd op de flash en 1.81 betekent dat deelnemer 1b in 1.81 seconden heeft
# gereageerd als er 1 flash en 1 beep was, in sociale conditie 1. Anders verwoord, dit is de gemiddelde reactietijd van de deelnemer 1a
# en 1b in sociale conditie 1, op 1 flash en 1 beep.

### DEEL 3
dataset %<>%
  group_by(participant) %>% 
  mutate(correct_a = equals(flashes, antwoord_flash_a),
         correct_b = equals(flashes, antwoord_flash_b),
         correct_ab = coalesce(correct_a, correct_b)) %>%
  ungroup()

dataset %>%
  group_by(participant) %>%
  group_map(~shapiro.test(as.numeric(.[['flashes']])))

dataset %<>% mutate(antwoord_flash_ab = coalesce(antwoord_flash_a, antwoord_flash_b))
dataset$correct_ab <- as.numeric(dataset$correct_ab)
dataset$sociale_condities <- as.character(dataset$sociale_condities)
dataset %<>% mutate(vision = ifelse(equals(beep == 2, flashes == 1), 1, 0))

dataset %>%
  ungroup() %>%
  write.xlsx("dataset_new_cols.xlsx", showNA = FALSE)

dataset %>%
  filter(participant != 1) %>%
  group_by(beep) %>%
  group_map(~ stats::aov(antwoord_flash_ab ~ sociale_condities, data = .) %>%
    supernova::pairwise(correction = "Bonferroni", alpha = 0.05, plot = TRUE)) ->
  beep_1_2

beep_1_2 %>% .[[1]] %>% .[1] %>% write.xlsx("antwoord_flash_ab_per_sociale_conditie_beep_1.xlsx", showNA = FALSE)
beep_1_2 %>% .[[2]] %>% .[1] %>% write.xlsx("antwoord_flash_ab_per_sociale_conditie_beep_2.xlsx", showNA = FALSE)


dataset %>%
  filter(participant != 1) %>%
  group_by(beep) %>% group_data()


dataset %>%
  filter(participant != 1) %>%
  group_by(participant) %>%
  mutate(
    correct_a_perc = correct_a %>%
      {
        `/`(sum(.[!is.na(.)]), length(.[!is.na(.)]))
      },
    correct_b_perc = correct_b %>%
      {
        `/`(sum(.[!is.na(.)]), length(.[!is.na(.)]))
      }
  ) %>%
  ungroup() %>%
  select(participant, correct_a_perc, correct_b_perc) %>%
  distinct() %>%
  write.xlsx("correct_a_b_perc.xlsx", showNA = FALSE)
