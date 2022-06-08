
library(haven)
library(tidyverse)
library(magrittr)
library(readxl)

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
dataset <- xlsx_to_df("Dataset.xlsx")
proef_dataset <- xlsx_to_df("PROEF dataset.xlsx")

#
# opvragen van de frequentieverdeling van geslacht
# om te kijken of er missings zijn
#
dataset %>%
  select(geslacht) %>%
  table()

#
# de functie mutate() voegt een nieuwe kolom "vrouw" toe aan de dataset.
# De waarde in de kolom "vrouw" wordt bepaald door een ifelse() statement.
# Als de waarde in de kolom "geslacht" gelijk is aan "v", wordt de waarde
# in de kolom "vrouw" gelijk aan 1. In alle andere gevallen wordt de
# waarde in de kolom "vrouw" gelijk aan 0. De kolom bevat geen missende waarden.
#
dataset %<>%
  mutate(vrouw = ifelse(geslacht == "v", 1, 0))

#
# deze functie roept de functie mutate() op en zet de kolommen
# met "pre" of "post" in de naam om naar integers met behulp van de
# funtie str_squish() en grepl() waarbij str_squish() ervoor zorgt dat er geen
# spaties in de data staan en grepl() kijkt welke cellwaarden voldoen aan de
# voorwaarde "^[0-2]$"en betekent dat de kolomwaarde alleen 0, 1 of 2 mag zijn.
# de symbolen ^ en $ staan voor de start en eindwaarde van de kolom
# respectievelijk. Bij een waarde die niet voldoet aan de voorwaarde krijgt
# deze de waarde NA_integer_ (= missing).
#
dataset %<>%
  mutate(across(matches("pre|post"), ~ str_squish(.x) %>%
    {
      ifelse(grepl("^[0-2]$", x = .), ., NA_integer_)
    } %>%
    as.integer()))

#
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
#
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


#
# opslaan als .sav bestand zonder kolom met de naam `115` (is denk ik een fout)
#
dataset_2 %<>% select(!`115`)
haven::write_sav(dataset_2, "dataset_2.sav")

#
# functie voor het uitvoeren van de betrouwbaarheidsanalyse
# over een select kolommen met naam gelijk aan '.name'
#
alfa <- function(.name, .df = dataset_2) {
  .df %>%
    .[grepl(.name, names(.))] %>%
    psych::alpha(check.keys = TRUE) %>%
    .[1]
}

#
# toepassen van bovenstaande functie
# en zorgt voor overzichtelijke samenvatting van de uitkomsten
#
c("r_pre_wst", "r_pre_tak", "r_post_wst", "r_post_tak") %>%
  list(map_df(., ~ alfa(.x)) %>%
    unnest(cols = c(total)) %>%
    set_names(gsub("[^a-zA-Z0-9]", "_", names(.)))) %>%
  reduce(cbind)

# de functie 'make_scale' maakt een nieuwe kolom met de som van alle kolommen in de dataset die beginnen met 'colname'.
#
# 'dataset_2 %>%' gebruikt de dataset_2 als input voor de functie.
# 'group_by(id_leerling)' geeft aan dat de functie 'make_scale' moet worden uitgevoerd voor elke 'id_leerling'.
# 'group_split()' splitst de dataset op in groepen.
# 'c(.[1], scale = {' maakt een nieuwe kolom met de som van alle kolommen in de dataset die beginnen met 'colname'.
# 'unlist(.) %>%' gebruikt de functie 'unlist' om alle kolommen in de dataset om te zetten in een lijst.
# '.[-1] %>%' gebruikt de functie 'unlist' om alle kolommen in de dataset om te zetten in een lijst.
# 'as.numeric() %>%' gebruikt de functie 'as.numeric' om alle kolommen in de dataset om te zetten in een numerieke waarde.
# 'sum() %>%' gebruikt de functie 'sum' om alle kolommen in de dataset op te tellen.
# 'ifelse(`==`(0, .), NA_real_, .)' gebruikt de functie 'ifelse' om een 'NA' waarde te maken
#  voor alle kolommen in de dataset waarvan de som gelijk is aan 0.
# 'c("id_leerling", paste0(.colname, "_sum"))' geeft de namen van de kolommen in de dataset.
#
make_scale <- function(.colname) {
  dataset_2 %>%
    group_by(id_leerling) %>%
    select(starts_with(.colname)) %>%
    group_split() %>%
    map_df(~ c(.[1], scale = {
      unlist(.) %>%
        .[-1] %>%
        as.numeric() %>%
        sum() %>%
        {
          ifelse(`==`(0, .), NA_real_, .)
        }
    })) %>%
    set_names(c("id_leerling", paste0(.colname, "_sum")))
}

#
# de code maakt van de variabelen "r_pre_wst", "r_pre_tak", "r_post_wst", "r_post_tak" schaalvariabelen.
# Vervolgens maakt het een lijst van de schaalvariabelen en de dataset_2.
# Tot slot wordt de dataset_2 met de schaalvariabelen samengevoegd.
#
dataset_3 <-
  c("r_pre_wst", "r_pre_tak", "r_post_wst", "r_post_tak") %>%
  map(~ make_scale(.)) %>%
  c(list(dataset_2)) %>%
  reduce(full_join)

#
# sla dataset_3 op als een SPSS bestand met de naam "dataset_3.sav"
#
haven::write_sav(dataset_3, "dataset_3.sav")

#
# de code maakt een functie genaamd "add_leeftijd"
# deze functie maakt van een kolom met geboortedatums de leeftijd op een bepaalde datum.
# de datum moet meegegeven worden als parameter, bijv. yyyymmdd = "2019-01-01"
#  de andere parameter .df is de dataset waarop de functie moet toegepast worden.
# leeftijd wordt berekend door van de geboortedatum de datum af te trekken en vervolgens
#  de leeftijd in jaren te berekenen.
# vervolgens worden de kolommen "geboortedatum" en "leeftijd" toegevoegd aan de oorspronkelijke dataset
# de uitdraai van de functie is de oorspronkelijke dataset met de kolom "leeftijd" achter "geboortedatum"
#
add_leeftijd <- function(.df, yyyymmdd) {
  .df %>%
    select(geboortedatum) %>%
    map_dfc(~ {
      lubridate::as_date(.) -
        as_date(as.character(yyyymmdd))
    } %>%
      as.numeric() %>%
      `/`(365) %>%
      abs() %>%
      unlist() %>%
      unname() %>%
      as.numeric()) %>%
    set_names("leeftijd") %>% 
    add_column(.df, ., .after = "geboortedatum")
}

dataset_4 <- add_leeftijd(dataset_3, "2019-01-01")


haven::write_sav(dataset_4, "dataset.sav")
