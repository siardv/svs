

library(haven)
library(tidyverse)
library(magrittr)
library(readxl)

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

dataset <-
  xlsx_to_df("Data experiment.xlsx")

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

more_than_2 <-
  dataset %>%
  select(antwoord_flashes) %>%
  unlist() %>%
  str_split("") %>%
  map(~ keep(., grepl("1|2|7|8", .))) %>%
  {
    which(lengths(.) > 2)
  }

# dataset %>% select(antwoord_flashes) %>% unlist() %>% str_split("") %>% map(~discard(., grepl("\\[|\\]|\\'|\"| ", .)))

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
  dataset$antwoord_flash_reactietijd_b1 <-
  dataset$antwoord_flash_reactietijd_a2 <-
  dataset$antwoord_flash_reactietijd_a2 <-
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

afrt <- dataset$antwoord_flash_reactietijd %>%
  {
    gsub("\\[([0-9]+.[0-9]+)\\]", "\\1", .)
  } %>%
  map_dbl(~ as.numeric(.))

for (i in 1:length(sc)) {
  cat("row: ", i, "\r")
  if (!is.na(cs[i])) {
    if (cs[i] == 1) {
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


####### DEEL 2 #######

dataset <- haven::read_sav("df_2022-06-07.sav")

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
    unlist() %>% is.na() %>% which() %>% length() == 2) {
    
    reactie_resp <-
      dataset$antwoord_communicatie[i] %>%
      str_extract_all("[0-9]", simplify = TRUE) %>%
      {
        gsub("0", "10", .)
      } %>%
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
      .[[1]]
    reactie_a_b <-
      reactie_tijd[sort(reactie_resp, index.return = TRUE)[[2]]]
    
    dataset$antwoord_communicatie_reactietijd_a2[i] <- as.numeric(reactie_a_b[1])
    dataset$antwoord_communicatie_reactietijd_b2[i] <- reactie_a_b[2]
  }
}

dataset$antwoord_communicatie_reactietijd_a <-
  map2(
    dataset$antwoord_communicatie_reactietijd_a1,
    dataset$antwoord_communicatie_reactietijd_a2, ~ ifelse(is.na(.x), .y, .x)
  ) %>% unlist()

dataset$antwoord_communicatie_reactietijd_b <-
  map2(
    dataset$antwoord_communicatie_reactietijd_b2,
    dataset$antwoord_communicatie_reactietijd_b2, ~ ifelse(is.na(.x), .y, .x)
  ) %>% unlist()

dataset %<>% select(
  participant,
  sociale_condities,
  beep,
  flashes,
  communicatie,
  antwoord_flashes,
  participant_a,
  participant_b,
  participant_a2,
  participant_b2,
  antwoord_flash_reactietijd,
  antwoord_flash_reactietijd_a,
  antwoord_flash_reactietijd_b,
  antwoord_communicatie,
  antwoord_communicatie_a,
  antwoord_communicatie_b,
  antwoord_communicatie_en_reactietijd,
  antwoord_communicatie_reactietijd_a,
  antwoord_communicatie_reactietijd_b
)

haven::write_sav(dataset, paste0("df_", Sys.Date(), ".sav"))
xlsx::write.xlsx(dataset, paste0("df_", Sys.Date(), ".xlsx"))
