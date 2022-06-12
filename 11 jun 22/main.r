
library(haven)
library(tidyverse)
library(magrittr)
library(readxl)
library(lubridate)

#
# functie fix_names() maakt van alle kolomnamen in een dataframe
# kleine letters en vervangt spaties door underscores.
#
fix_names <- function(.df) {
  .df %>% set_names(names(.) %>%
    tolower() %>%
    map_chr(~ str_replace_all(
      .,
      c("(\\(.*\\))" = "", "[.]+" = " ", "\\?|\\;" = "")
    ) %>%
      {
        gsub("\\s", "_", str_squish(.))
      }))
}

#
# functie xlsx_to_df() leest een .xlsx-bestand in en converteert
# het bestand naar een dataframe.
# de naam van de kolommen worden aangepast met de functie fix_names()
#
xlsx_to_df <- function(.file) {
  paste0(getwd(), "/", .file) %>%
    read_excel(col_names = TRUE, trim_ws = TRUE) %>%
    fix_names()
}

#
# de 2 nieuwe functies worden uitgevoerd op de twee .xlsx-bestanden.
# de uitkomst wordt opgeslagen in de variabelen 'dataset' en 'proef_dataset'
#
# dataset <- xlsx_to_df("Dataset.xlsx")
# proef_dataset <- xlsx_to_df("PROEF dataset.xlsx")

dataset <-
  xlsx_to_df("Dataset nieuw.xlsx")

dataset %<>%
  mutate(vrouw = ifelse(geslacht == "v", 1, 0))

dataset %<>%
  mutate(across(matches("pre|post"), ~ str_squish(.x) %>%
    {
      ifelse(grepl("^[0-2]$", x = .), ., NA_integer_)
    } %>%
    as.integer()))

dataset %<>%
  mutate(
    across(
      starts_with("pre_wst"),
      ~ if (sum(is.na(.)) < 3) {
        replace(., is.na(.), mean(., na.rm = TRUE))
      } else {
        .
      }
    ),
    across(
      starts_with("pre_tak"),
      ~ if (sum(is.na(.)) < 3) {
        replace(., is.na(.), mean(., na.rm = TRUE))
      } else {
        .
      }
    ),
    across(
      starts_with("post_wst"),
      ~ if (sum(is.na(.)) < 3) {
        replace(., is.na(.), mean(., na.rm = TRUE))
      } else {
        .
      }
    ),
    across(
      starts_with("post_tak"),
      ~ if (sum(is.na(.)) < 3) {
        replace(., is.na(.), mean(., na.rm = TRUE))
      } else {
        .
      }
    )
  )

alfa <- function(.name, .df = dataset) {
  .df %>%
    .[grepl(.name, names(.))] %>%
    psych::alpha(check.keys = TRUE) %>%
    .[1]
}

c("pre_wst", "pre_tak", "post_wst", "post_tak") %>%
  list(map_df(., ~ alfa(.x)) %>%
    unnest(cols = c(total)) %>%
    set_names(gsub("[^a-zA-Z0-9]", "_", names(.)))) %>%
  reduce(cbind)

dataset %<>%
  mutate(
    pre_wst_sum = rowSums(.[startsWith(names(.), "pre_wst")]),
    pre_tak_sum = rowSums(.[startsWith(names(.), "pre_tak")]),
    post_wst_sum = rowSums(.[startsWith(names(.), "post_wst")]),
    post_tak_sum = rowSums(.[startsWith(names(.), "post_tak")])
  )

dataset$geboortedatum[which(grepl("^30.*?$", dataset$geboortedatum))] %<>% {
  gsub("^30", "20", .)
}

dataset %<>%
  add_column(ronde_date = dataset$ronde %>%
    {
      ifelse(equals(., 1), "2022-01-15", "2022-03-15")
    }, .after = "ronde")

dataset$leeftijd <-
  magrittr::subtract(
    lubridate::as_date(dataset$ronde_date),
    lubridate::as_date(dataset$geboortedatum)
  ) %>%
  divide_by(365) %>%
  as.numeric()

dataset %<>% mutate(experiment = case_when(
  groep_1 == "experimenteel" ~ 1,
  groep_1 == "controle" ~ 0,
  TRUE ~ NA_real_
))

dataset$experiment %<>%
  haven::labelled_spss(c(
    experimenteel = 1,
    controle = 0
  ))

dataset %<>% mutate(school_id = case_when(
  school == "Twaalfruiter" ~ 0,
  school == "De Mijlpaal" ~ 1,
  school == "Drie Koningen" ~ 2,
  school == "KC Haarzicht" ~ 3,
  school == "Kwartiermaker" ~ 4,
  TRUE ~ NA_real_
))

dataset$school_id %<>%
  haven::labelled_spss(c(
    Twaalfruiter = 0,
    DeMijlpaal = 1,
    DrieKoningen = 2,
    KCHaarzicht = 3,
    Kwartiermaker = 4
  ))

dataset$id_leerling %<>% {
  gsub("[A-Z]", "", .)
}

dataset %>%
  filter(toestemming_testjes == 1) %>% 
  select(!c(
    groep_1,
    naam_leerling,
    leerkracht,
    geslacht,
    ronde_date,
    school
  )) -> dataset_temp

write_sav(dataset_temp, "dataset_11_06.sav")

# 37 cases filtered out
