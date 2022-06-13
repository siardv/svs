
library(haven)
library(tidyverse)
library(magrittr)
library(readxl)
library(lubridate)
library(lavaanPlot)
library(sem)
library(semPlot)

options(scipen = 999)

###########  1. DATA LADEN

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

dataset <- xlsx_to_df("Dataset nieuw.xlsx")

###########  1. DATA HERCODEREN

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


###########  3. SUBSCHALEN MAKEN
dataset %<>%
  mutate(
    pre_wst_sum = rowSums(.[startsWith(names(.), "pre_wst")]),
    pre_tak_sum = rowSums(.[startsWith(names(.), "pre_tak")]),
    post_wst_sum = rowSums(.[startsWith(names(.), "post_wst")]),
    post_tak_sum = rowSums(.[startsWith(names(.), "post_tak")])
  )

###########  4. BETROUWBAARHEID ANALYSE

# Reliability analysis
alfa <- function(.name, .df = dataset) {
  .df %>%
    .[grepl(.name, names(.))] %>%
    psych::alpha(check.keys = TRUE) %>%
    .[1]
}
c("pre_wst_\\d", "pre_tak_\\d", "post_wst_\\d", "post_tak_\\d") %>%
  list(map_df(., ~ alfa(.x)) %>%
    unnest(cols = c(total)) %>%
    set_names(gsub("[^a-zA-Z0-9]", "_", names(.)))) %>%
  reduce(cbind)

# Shapiro-Wilk normality test
shapiro.test(dataset$post_wst_sum)
shapiro.test(dataset$post_tak_sum)

# Levene's Test for Homogeneity of Variance
car::leveneTest(dataset$pre_tak_sum, group = dataset$experiment, center = mean)
car::leveneTest(dataset$post_tak_sum, group = dataset$experiment, center = mean)
car::leveneTest(dataset$pre_wst_sum, group = dataset$experiment, center = mean)
car::leveneTest(dataset$post_wst_sum, group = dataset$experiment, center = mean)

###########  5. CASES SELECTEREN (CASES FOR ANALYSIS)
# 37 cases filtered out
dataset %<>%
  filter(toestemming_testjes == 1) %>%
  select(!c(
    groep_1,
    naam_leerling,
    leerkracht,
    geslacht,
    ronde_date,
    school
  ))


write_sav(dataset, "dataset.sav")

###########  6. REGRESSIE

model_1a <- lm(post_tak_sum ~ pre_tak_sum + experiment, data = dataset)
model_2a <- lm(post_tak_sum ~ pre_tak_sum + experiment + leeftijd + vrouw, data = dataset)
model_3a <- lm(post_tak_sum ~ pre_tak_sum + experiment + leeftijd + vrouw + experiment * leeftijd, data = dataset)
model_1b <- lm(post_wst_sum ~ pre_wst_sum + experiment, data = dataset)
model_2b <- lm(post_wst_sum ~ pre_wst_sum + experiment + leeftijd + vrouw, data = dataset)
model_3b <- lm(post_wst_sum ~ pre_wst_sum + experiment + leeftijd + vrouw + experiment * leeftijd, data = dataset)

summary(model_1a, digits = 3)
summary(model_2a, digits = 3)
summary(model_3a, digits = 3)
summary(model_1b, digits = 3)
summary(model_2b, digits = 3)
summary(model_3b, digits = 3)

# descriptive statistics (LaTex output)
dataset %>%
  select(pre_tak_sum, pre_wst_sum, post_tak_sum, post_wst_sum, experiment, leeftijd, vrouw) %>%
  descriptr::ds_tidy_stats() %>%
  select(vars, min, max, mean, median, variance, stdev) %>% set_names(names(.) %>% str_to_title() %>% .[-1] %>% {c("", .)}) %>% 
  xtable::xtable()

wst_model <- "post_wst_sum ~ pre_wst_sum + experiment + leeftijd + vrouw + experiment:leeftijd"
wst <- sem(wst_model, group = "experiment", data = dataset)
labels_wst <- list(post_wst_sum = "post-test (woordenschat)", pre_wst_sum = "pre-test (woordenschat)", experiment = "interventie", leeftijd = "leeftijd", vrouw = "vrouw")
lavaan_wst <- lavaanPlot(model = wst, labels = labels_wst, coefs = TRUE, covs = FALSE, stand = FALSE, sig = TRUE, stars = "regress", graph_options = list(rankdir = "RL"))
lavaanPlot::save_png(lavaan_wst, "model_3b.png")

tak_model <- "post_tak_sum ~ pre_tak_sum + experiment + leeftijd + vrouw + experiment:leeftijd"
tak <- sem(tak_model, data = dataset)
labels_tak <- list(post_tak_sum = "post-test (narratieve competentie)", pre_tak_sum = "pre-test (narratieve competentie)", experiment = "interventie", leeftijd = "leeftijd", vrouw = "vrouw")
lavaan_tak <- lavaanPlot(model = tak, labels = labels_tak, coefs = TRUE, covs = FALSE, stand = FALSE, sig = TRUE, stars = "regress", graph_options = list(rankdir = "RL"))
lavaanPlot::save_png(lavaan_tak, "model_3a.png")

# plot(dataset$pre_wst_sum, dataset$post_wst_sum)
# plot(dataset$pre_tak_sum, dataset$post_tak_sum)

# alternatieve plots 
semPaths(model_3a, what = "path", whatLabels = "par", edge.color = "black", layout = "tree2", intercepts = FALSE, rotation = 2)
semPaths(model_3b, what = "path", whatLabels = "par", edge.color = "black", layout = "tree2", intercepts = FALSE, rotation = 2)

