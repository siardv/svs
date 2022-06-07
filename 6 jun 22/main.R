library(haven)
library(tidyverse)
library(magrittr)
library(readxl)


# functie fix_names() maakt van alle kolomnamen in een dataframe
# kleine letters en vervangt spaties door underscores.
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

# functie xlsx_to_df() leest een .xlsx-bestand in en converteert
# het bestand naar een dataframe.
# de naam van de kolommen worden aangepast met de functie fix_names()
xlsx_to_df <- function(.file) {
  paste0(getwd(), "/", .file) %>%
    read_excel(col_names = TRUE, trim_ws = TRUE) %>%
    fix_names()
}

# de 2 nieuwe functies worden uitgevoerd op de twee .xlsx-bestanden.
# de uitkomst wordt opgeslagen in de variabelen 'dataset' en 'proef_dataset'
dataset <- xlsx_to_df("Dataset.xlsx")
proef_dataset <- xlsx_to_df("PROEF dataset.xlsx")

# opvragen van de frequentieverdeling van geslacht
# om te kijken of er missings zijn
dataset %>%
  select(geslacht) %>%
  table()

# de functie mutate() voegt een nieuwe kolom "vrouw" toe aan de dataset.
# De waarde in de kolom "vrouw" wordt bepaald door een ifelse() statement.
# Als de waarde in de kolom "geslacht" gelijk is aan "v", wordt de waarde
# in de kolom "vrouw" gelijk aan 1. In alle andere gevallen wordt de
# waarde in de kolom "vrouw" gelijk aan 0. De kolom bevat geen missende waarden.
dataset %<>%
  mutate(vrouw = ifelse(geslacht == "v", 1, 0))

# deze functie roept de functie mutate() op en zet de kolommen
# met "pre" of "post" in de naam om naar integers met behulp van de
# funtie str_squish() en grepl() waarbij str_squish() ervoor zorgt dat er geen
# spaties in de data staan en grepl() kijkt welke cellwaarden voldoen aan de
# voorwaarde "^[0-2]$"en betekent dat de kolomwaarde alleen 0, 1 of 2 mag zijn.
# de symbolen ^ en $ staan voor de start en eindwaarde van de kolom
# respectievelijk. Bij een waarde die niet voldoet aan de voorwaarde krijgt
# deze de waarde NA_integer_ (= missing).
dataset %<>%
  mutate(across(matches("pre|post"), ~ str_squish(.x) %>%
    {
      ifelse(grepl("^[0-2]$", x = .), ., NA_integer_)
    } %>%
    as.integer()))

# deze functie selecteert eerst alle kolommen die bijv. 'pre_tak' in de naam hebben,
# vervolgens wordt berekend hoeveel missing values er per rij zijn en wordt
# dit weergegeven als een nieuwe kolom, met de naam 'missing_count'.
# Vervolgens wordt er gekeken naar de rijen met minder dan 2 missing values
# en wordt er voor de kolommen die 'pre_tak' in de naam hebben, berekend
# wat het gemiddelde is van de waardes in die kolommen. Dit gemiddelde wordt
# dan ingevuld in de plekken waar er missing values waren, zodat per rij weer
# alle waardes ingevuld zijn.
# De nieuwe kolommen die hieruit komen krijgen een 'r_' voor de naam,
# en toegevoegd aan de oorpsronkelijke dataset. Het behouden van de
# originele kolommen is niet per se nodig, maar wel handig om te zien
# wat precies vervangen is en wat niet.
replace_na_if <- function(.pattern, .df = dataset) {
  .df %>%
    select(id_leerling, contains(.pattern)) %>%
    mutate(missing_count = rowSums(is.na(.))) %>%
    group_by(missing_count) %>%
    filter(missing_count < 2) %>%
    mutate_at(
      vars(contains(.pattern)),
      list(~ replace(., is.na(.), mean(., na.rm = TRUE)))
    ) %>%
    ungroup() %>%
    select(!missing_count) %>%
    set_names({
      c(
        names(.[, 1]),
        names(.[, -1]) %>% paste0("r_", .)
      )
    })
}

c("pre_wst", "pre_tak", "post_wst", "post_tak") %>%
  map(~ replace_na_if(.)) %>%
  c(list(dataset)) %>%
  rev() %>%
  reduce(full_join) -> dataset_2

# opslaan als .sav bestand zonder var `115` (is denk ik een fout)
dataset_2 %<>% select(!`115`)
haven::write_sav(dataset_2, "dataset_2.sav")

# betrouwbaarheidsanalyse over een reeks kolommen
alfa <- function(.name, .df = dataset_2) {
  .df %>%
    .[grepl(.name, names(.))] %>%
    psych::alpha(check.keys = TRUE) %>% .[1]
}

# toepassen van bovenstaande functie
# en zorgt voor overzichtelijke samenvatting van de uitkomsten
c("r_pre_wst", "r_pre_tak", "r_post_wst", "r_post_tak") %>%
  list(map_df(., ~ alfa(.x)) %>%
    unnest(cols = c(total)) %>%
    set_names(gsub("[^a-zA-Z0-9]", "_", names(.)))) %>%
  reduce(cbind)



