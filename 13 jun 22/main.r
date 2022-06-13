
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

school_dummy <- dataset_temp$school_id %>% psych::dummy.code()

set_names(
  as.data.frame(school_dummy),
  names(attr(dataset_temp$school_id, "labels"))[match(
    as.numeric(names(as.data.frame(school_dummy))),
    attr(dataset_temp$school_id, "labels")
  )]) -> school_dummy

datasetx <-
  bind_cols(dataset_temp, school_dummy)

write_sav(datasetx, "datasetx.sav")

#     Twaalfruiter + DrieKoningen + Kwartiermaker + KCHaarzicht + DeMijlpaal


model_1a <- lm(post_tak_sum ~ pre_tak_sum + experiment, data = dataset_temp)
summary(model_1a)
model_2a <- lm(post_tak_sum ~ pre_tak_sum + experiment + leeftijd + vrouw, data = dataset_temp) 
summary(model_2a)
model_3a <- lm(post_tak_sum ~ pre_tak_sum + experiment + leeftijd + vrouw + pre_tak_sum*vrouw, data = dataset_temp)
summary(model_3a)




sink("kk")
cat("model_1a\n")
model_1a <- lm(post_tak_sum ~ pre_tak_sum + experiment, data = dataset_temp)
summary(model_1a)
cat("----------------------\n")

cat("model_2a\n")
model_2a <- lm(post_tak_sum ~ pre_tak_sum + experiment + leeftijd + vrouw, data = dataset_temp) 
summary(model_2a)
cat("----------------------\n")

cat("model_3a\n")
model_3a <- lm(post_tak_sum ~ pre_tak_sum + experiment + leeftijd + vrouw + pre_tak_sum*vrouw, data = dataset_temp)
summary(model_3a)
cat("----------------------\n")

cat("model_2a vs model_3a\n")
anova(model_2a, model_3a)

cat("----------------------\n")
cat("----------------------\n")

cat("model_1b\n")
model_1b <- lm(post_wst_sum ~ pre_wst_sum + experiment, data = dataset_temp)
summary(model_1b)
cat("----------------------\n")

cat("model_2b\n")
model_2b <- lm(post_wst_sum ~ pre_wst_sum + experiment + leeftijd + vrouw, data = dataset_temp) 
summary(model_2b)
cat("----------------------\n")

cat("model_3b\n")
model_3b <- lm(post_wst_sum ~ pre_wst_sum + experiment + leeftijd + vrouw + pre_wst_sum*vrouw, data = dataset_temp)
summary(model_3b)
cat("----------------------\n")

cat("model_2b vs model_3b\n")
anova(model_2b, model_3b)
sink()


library(lavaan)
library(lavaanPlot)
wst_model <- 'post_wst_sum ~ pre_wst_sum + experiment + leeftijd + vrouw
          pre_wst_sum ~ vrouw'
wst <- sem(wst_model, data = dataset_temp)
labels_wst <- list(post_wst_sum = "post-test WST", pre_wst_sum = "per-test WST", experiment = "interventie", leeftijd = "leeftijd", vrouw = "vrouw") 
lavaan_wst <- lavaanPlot(model = wst, labels = labels_wst, coefs = TRUE, covs=F, stand = FALSE, sig = TRUE, stars = "regress", graph_options = list(rankdir = "RL") )

tak_model <- 'post_tak_sum ~ pre_tak_sum + experiment + leeftijd + vrouw
              pre_tak_sum ~ vrouw'
tak <- sem(tak_model, data = dataset_temp)
labels_tak <- list(post_tak_sum = "post-test TAK", pre_tak_sum = "per-test TAK", experiment = "interventie", leeftijd = "leeftijd", vrouw = "vrouw") 
lavaan_tak <- lavaanPlot(model = tak, labels = labels_tak, coefs = TRUE, covs=F, stand = FALSE, sig = TRUE, stars = "regress", graph_options = list(rankdir = "RL") )