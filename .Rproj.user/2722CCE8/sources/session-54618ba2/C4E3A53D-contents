library(tidyverse)
library(haven)
library(ltm)
library(MASS)
library(broom)
library(mfx)
library(texreg)
library(sjPlot)
library(officer)
library(broom)
library(flextable)

data_raw <- read_spss('data/gds2022_policy_45094.sav')

data <- 
  data_raw %>%
  mutate(scen1_immoral = case_when(scen1immoral == 4 | scen1immoral == 5 ~ "Immoral",
                                   scen1immoral == 3 ~ "Neutral",
                                   scen1immoral == 1 | scen1immoral == 2 ~ "Moral"),
         scen2_immoral = case_when(scen2immoral == 4 | scen2immoral == 5 ~ "Immoral",
                                   scen2immoral == 3 ~ "Neutral",
                                   scen2immoral == 1 | scen2immoral == 2 ~ "Moral"),
         scen3_immoral = case_when(scen3immoral == 4 | scen3immoral == 5 ~ "Immoral",
                                   scen3immoral == 3 ~ "Neutral",
                                   scen3immoral == 1 | scen3immoral == 2 ~ "Moral"),
         scen4_immoral = case_when(scen4immoral == 4 | scen4immoral == 5 ~ "Immoral",
                                   scen4immoral == 3 ~ "Neutral",
                                   scen4immoral == 1 | scen4immoral == 2 ~ "Moral"),
         scen5_immoral = case_when(scen5immoral == 4 | scen5immoral == 5 ~ "Immoral",
                                   scen5immoral == 3 ~ "Neutral",
                                   scen5immoral == 1 | scen5immoral == 2 ~ "Moral"),
         scen6_immoral = case_when(scen6immoral == 4 | scen6immoral == 5 ~ "Immoral",
                                   scen6immoral == 3 ~ "Neutral",
                                   scen6immoral == 1 | scen6immoral == 2 ~ "Moral")) %>%
  mutate(across(c(scen1supp, scen2supp, scen3supp, scen4supp, scen5supp, scen6supp), ~as_factor(.))) %>%
  mutate(across(c(scen1crime:scen1trust, scen2crime:scen2trust, scen3crime:scen3trust, scen4crime:scen4trust, scen5crime:scen5trust, scen6crime:scen6trust), 
                ~ case_when(. == 4 | . == 5 ~ T, T ~ F))) %>%
  mutate(male = gender == 1,
         white = ethnicity == 1,
         age = case_when(age > 85 ~ NA_real_, TRUE ~ age)) %>%
  mutate(age = as.numeric(age)) %>%
  filter(ncountry == 10 | ncountry == 13 | ncountry == 14 | ncountry == 21 | ncountry == 25 | ncountry == 38 | ncountry == 47 | ncountry == 58 | ncountry == 72 | ncountry == 73 | ncountry == 80
         | ncountry == 98 | ncountry == 104 | ncountry == 106 | ncountry == 140 | ncountry == 155 | ncountry == 173 | ncountry == 174 | ncountry == 201 | ncountry == 206 | ncountry == 208
         | ncountry == 228 | ncountry == 227) %>%
  mutate(country = as_factor(scountry)) %>%
  mutate(arrest = case_when(
    polactionsarrusecan == 1 | polactionsarrusecan == 2~ T,
    polactionsarrsupcan == 1 | polactionsarrsupcan == 2 ~ T,
    polactionsarruseoth == 1 | polactionsarruseoth == 2 ~ T,
    polactionsarrsupoth == 1 | polactionsarrsupoth == 2 ~ T,
    polactionsconvuse == 1 | polactionsconvuse == 2 ~ T,
    polactionsconvsup == 1 | polactionsconvsup == 2 ~ T,
    TRUE ~ FALSE
  )) %>%
  mutate(purchase = as_factor(purchase))

# Function to extract required details from a model
extract_coefficients <- function(model) {
  coef_summary <- summary(model)$coefficients
  tibble(
    Coefficient = coef_summary[1, 1],
    Std_Error = coef_summary[1, 2],
    t_value = coef_summary[1, 3]
  )
}

# change ref groups
data_immoral <- data %>% mutate(across(c(scen1_immoral:scen6_immoral), ~factor(., levels = c('Immoral', 'Neutral', 'Moral'))))
data_neutral <- data %>% mutate(across(c(scen1_immoral:scen6_immoral), ~factor(., levels = c('Neutral', 'Immoral', 'Moral'))))
data_moral <- data %>% mutate(across(c(scen1_immoral:scen6_immoral), ~factor(., levels = c('Moral', 'Immoral', 'Neutral'))))

# ref: immoral
## aproach would reduce crime
m_scen1_immoral_crime <- polr(scen1supp ~ scen1crime * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_crime <- polr(scen2supp ~ scen2crime * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_crime <- polr(scen3supp ~ scen3crime * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_crime <- polr(scen4supp ~ scen4crime * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_crime <- polr(scen5supp ~ scen5crime * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_crime <- polr(scen6supp ~ scen6crime * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## approach would encourage drug use
m_scen1_immoral_encour <- polr(scen1supp ~ scen1encour * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_encour <- polr(scen2supp ~ scen2encour * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_encour <- polr(scen3supp ~ scen3encour * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_encour <- polr(scen4supp ~ scen4encour * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_encour <- polr(scen5supp ~ scen5encour * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_encour <- polr(scen6supp ~ scen6encour * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## approach would restrict freedom
m_scen1_immoral_free <- polr(scen1supp ~ scen1free * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_free <- polr(scen2supp ~ scen2free * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_free <- polr(scen3supp ~ scen3free * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_free <- polr(scen4supp ~ scen4free * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_free <- polr(scen5supp ~ scen5free * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_free <- polr(scen6supp ~ scen6free * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_immoral_health <- polr(scen1supp ~ scen1health * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_health <- polr(scen2supp ~ scen2health * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_health <- polr(scen3supp ~ scen3health * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_health <- polr(scen4supp ~ scen4health * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_health <- polr(scen5supp ~ scen5health * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_health <- polr(scen6supp ~ scen6health * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_immoral_vulner <- polr(scen1supp ~ scen1vulner * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_vulner <- polr(scen2supp ~ scen2vulner * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_vulner <- polr(scen3supp ~ scen3vulner * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_vulner <- polr(scen4supp ~ scen4vulner * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_vulner <- polr(scen5supp ~ scen5vulner * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_vulner <- polr(scen6supp ~ scen6vulner * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## would boost the economy
m_scen1_immoral_econ <- polr(scen1supp ~ scen1econ * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_econ <- polr(scen2supp ~ scen2econ * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_econ <- polr(scen3supp ~ scen3econ * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_econ <- polr(scen4supp ~ scen4econ * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_econ <- polr(scen5supp ~ scen5econ * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_econ <- polr(scen6supp ~ scen6econ * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## would improve trust in govt
m_scen1_immoral_trust <- polr(scen1supp ~ scen1trust * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_trust <- polr(scen2supp ~ scen2trust * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_trust <- polr(scen3supp ~ scen3trust * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_trust <- polr(scen4supp ~ scen4trust * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_trust <- polr(scen5supp ~ scen5trust * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_trust <- polr(scen6supp ~ scen6trust * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

models_immoral <- list(m_scen1_immoral_crime, m_scen2_immoral_crime, m_scen3_immoral_crime, m_scen4_immoral_crime, m_scen5_immoral_crime, m_scen6_immoral_crime,
                       m_scen1_immoral_encour, m_scen2_immoral_encour, m_scen3_immoral_encour, m_scen4_immoral_encour, m_scen5_immoral_encour, m_scen6_immoral_encour,
                       m_scen1_immoral_free, m_scen2_immoral_free, m_scen3_immoral_free, m_scen4_immoral_free, m_scen5_immoral_free, m_scen6_immoral_free,
                       m_scen1_immoral_health, m_scen2_immoral_health, m_scen3_immoral_health, m_scen4_immoral_health, m_scen5_immoral_health, m_scen6_immoral_health,
                       m_scen1_immoral_vulner, m_scen2_immoral_vulner, m_scen3_immoral_vulner, m_scen4_immoral_vulner, m_scen5_immoral_vulner, m_scen6_immoral_vulner,
                       m_scen1_immoral_econ, m_scen2_immoral_econ, m_scen3_immoral_econ, m_scen4_immoral_econ, m_scen5_immoral_econ, m_scen6_immoral_econ,
                       m_scen1_immoral_trust, m_scen2_immoral_trust, m_scen3_immoral_trust, m_scen4_immoral_trust, m_scen5_immoral_trust, m_scen6_immoral_trust)

# Apply the function to each model in the list and bind rows
coefficients_data_immoral <- lapply(models_immoral, extract_coefficients) %>%
  bind_rows() %>%
  mutate(mor = "Immoral",
         scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))

m_scen1_neutral_crime <- polr(scen1supp ~ scen1crime * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_crime <- polr(scen2supp ~ scen2crime * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_crime <- polr(scen3supp ~ scen3crime * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_crime <- polr(scen4supp ~ scen4crime * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_crime <- polr(scen5supp ~ scen5crime * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_crime <- polr(scen6supp ~ scen6crime * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## approach would encourage drug use
m_scen1_neutral_encour <- polr(scen1supp ~ scen1encour * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_encour <- polr(scen2supp ~ scen2encour * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_encour <- polr(scen3supp ~ scen3encour * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_encour <- polr(scen4supp ~ scen4encour * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_encour <- polr(scen5supp ~ scen5encour * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_encour <- polr(scen6supp ~ scen6encour * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## approach would restrict freedom
m_scen1_neutral_free <- polr(scen1supp ~ scen1free * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_free <- polr(scen2supp ~ scen2free * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_free <- polr(scen3supp ~ scen3free * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_free <- polr(scen4supp ~ scen4free * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_free <- polr(scen5supp ~ scen5free * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_free <- polr(scen6supp ~ scen6free * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_neutral_health <- polr(scen1supp ~ scen1health * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_health <- polr(scen2supp ~ scen2health * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_health <- polr(scen3supp ~ scen3health * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_health <- polr(scen4supp ~ scen4health * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_health <- polr(scen5supp ~ scen5health * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_health <- polr(scen6supp ~ scen6health * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_neutral_vulner <- polr(scen1supp ~ scen1vulner * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_vulner <- polr(scen2supp ~ scen2vulner * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_vulner <- polr(scen3supp ~ scen3vulner * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_vulner <- polr(scen4supp ~ scen4vulner * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_vulner <- polr(scen5supp ~ scen5vulner * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_vulner <- polr(scen6supp ~ scen6vulner * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## would boost the economy
m_scen1_neutral_econ <- polr(scen1supp ~ scen1econ * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_econ <- polr(scen2supp ~ scen2econ * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_econ <- polr(scen3supp ~ scen3econ * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_econ <- polr(scen4supp ~ scen4econ * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_econ <- polr(scen5supp ~ scen5econ * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_econ <- polr(scen6supp ~ scen6econ * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## would improve trust in govt
m_scen1_neutral_trust <- polr(scen1supp ~ scen1trust * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_trust <- polr(scen2supp ~ scen2trust * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_trust <- polr(scen3supp ~ scen3trust * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_trust <- polr(scen4supp ~ scen4trust * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_trust <- polr(scen5supp ~ scen5trust * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_trust <- polr(scen6supp ~ scen6trust * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

models_neutral <- list(m_scen1_neutral_crime, m_scen2_neutral_crime, m_scen3_neutral_crime, m_scen4_neutral_crime, m_scen5_neutral_crime, m_scen6_neutral_crime,
                       m_scen1_neutral_encour, m_scen2_neutral_encour, m_scen3_neutral_encour, m_scen4_neutral_encour, m_scen5_neutral_encour, m_scen6_neutral_encour,
                       m_scen1_neutral_free, m_scen2_neutral_free, m_scen3_neutral_free, m_scen4_neutral_free, m_scen5_neutral_free, m_scen6_neutral_free,
                       m_scen1_neutral_health, m_scen2_neutral_health, m_scen3_neutral_health, m_scen4_neutral_health, m_scen5_neutral_health, m_scen6_neutral_health,
                       m_scen1_neutral_vulner, m_scen2_neutral_vulner, m_scen3_neutral_vulner, m_scen4_neutral_vulner, m_scen5_neutral_vulner, m_scen6_neutral_vulner,
                       m_scen1_neutral_econ, m_scen2_neutral_econ, m_scen3_neutral_econ, m_scen4_neutral_econ, m_scen5_neutral_econ, m_scen6_neutral_econ,
                       m_scen1_neutral_trust, m_scen2_neutral_trust, m_scen3_neutral_trust, m_scen4_neutral_trust, m_scen5_neutral_trust, m_scen6_neutral_trust)

# Apply the function to each model in the list and bind rows
coefficients_data_neutral <- lapply(models_neutral, extract_coefficients) %>%
  bind_rows() %>%
  mutate(mor = "Neutral",
         scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))

m_scen1_moral_crime <- polr(scen1supp ~ scen1crime * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_crime <- polr(scen2supp ~ scen2crime * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_crime <- polr(scen3supp ~ scen3crime * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_crime <- polr(scen4supp ~ scen4crime * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_crime <- polr(scen5supp ~ scen5crime * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_crime <- polr(scen6supp ~ scen6crime * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## approach would encourage drug use
m_scen1_moral_encour <- polr(scen1supp ~ scen1encour * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_encour <- polr(scen2supp ~ scen2encour * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_encour <- polr(scen3supp ~ scen3encour * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_encour <- polr(scen4supp ~ scen4encour * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_encour <- polr(scen5supp ~ scen5encour * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_encour <- polr(scen6supp ~ scen6encour * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## approach would restrict freedom
m_scen1_moral_free <- polr(scen1supp ~ scen1free * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_free <- polr(scen2supp ~ scen2free * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_free <- polr(scen3supp ~ scen3free * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_free <- polr(scen4supp ~ scen4free * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_free <- polr(scen5supp ~ scen5free * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_free <- polr(scen6supp ~ scen6free * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_moral_health <- polr(scen1supp ~ scen1health * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_health <- polr(scen2supp ~ scen2health * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_health <- polr(scen3supp ~ scen3health * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_health <- polr(scen4supp ~ scen4health * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_health <- polr(scen5supp ~ scen5health * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_health <- polr(scen6supp ~ scen6health * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_moral_vulner <- polr(scen1supp ~ scen1vulner * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_vulner <- polr(scen2supp ~ scen2vulner * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_vulner <- polr(scen3supp ~ scen3vulner * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_vulner <- polr(scen4supp ~ scen4vulner * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_vulner <- polr(scen5supp ~ scen5vulner * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_vulner <- polr(scen6supp ~ scen6vulner * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## would boost the economy
m_scen1_moral_econ <- polr(scen1supp ~ scen1econ * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_econ <- polr(scen2supp ~ scen2econ * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_econ <- polr(scen3supp ~ scen3econ * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_econ <- polr(scen4supp ~ scen4econ * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_econ <- polr(scen5supp ~ scen5econ * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_econ <- polr(scen6supp ~ scen6econ * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## would improve trust in govt
m_scen1_moral_trust <- polr(scen1supp ~ scen1trust * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_trust <- polr(scen2supp ~ scen2trust * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_trust <- polr(scen3supp ~ scen3trust * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_trust <- polr(scen4supp ~ scen4trust * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_trust <- polr(scen5supp ~ scen5trust * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_trust <- polr(scen6supp ~ scen6trust * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

models_moral <- list(m_scen1_moral_crime, m_scen2_moral_crime, m_scen3_moral_crime, m_scen4_moral_crime, m_scen5_moral_crime, m_scen6_moral_crime,
                     m_scen1_moral_encour, m_scen2_moral_encour, m_scen3_moral_encour, m_scen4_moral_encour, m_scen5_moral_encour, m_scen6_moral_encour,
                     m_scen1_moral_free, m_scen2_moral_free, m_scen3_moral_free, m_scen4_moral_free, m_scen5_moral_free, m_scen6_moral_free,
                     m_scen1_moral_health, m_scen2_moral_health, m_scen3_moral_health, m_scen4_moral_health, m_scen5_moral_health, m_scen6_moral_health,
                     m_scen1_moral_vulner, m_scen2_moral_vulner, m_scen3_moral_vulner, m_scen4_moral_vulner, m_scen5_moral_vulner, m_scen6_moral_vulner,
                     m_scen1_moral_econ, m_scen2_moral_econ, m_scen3_moral_econ, m_scen4_moral_econ, m_scen5_moral_econ, m_scen6_moral_econ,
                     m_scen1_moral_trust, m_scen2_moral_trust, m_scen3_moral_trust, m_scen4_moral_trust, m_scen5_moral_trust, m_scen6_moral_trust)

# Apply the function to each model in the list and bind rows
coefficients_data_moral <- lapply(models_moral, extract_coefficients) %>%
  bind_rows() %>%
  mutate(mor = "Moral",
         scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))

# ref: never arrested
## aproach would reduce crime
m_scen1_arrest_crime <- polr(scen1supp ~ scen1crime * arrest + age + male + country, data =data, method = 'logistic')
m_scen2_arrest_crime <- polr(scen2supp ~ scen2crime * arrest + age + male + country, data =data, method = 'logistic')
m_scen3_arrest_crime <- polr(scen3supp ~ scen3crime * arrest + age + male + country, data =data, method = 'logistic')
m_scen4_arrest_crime <- polr(scen4supp ~ scen4crime * arrest + age + male + country, data =data, method = 'logistic')
m_scen5_arrest_crime <- polr(scen5supp ~ scen5crime * arrest + age + male + country, data =data, method = 'logistic')
m_scen6_arrest_crime <- polr(scen6supp ~ scen6crime * arrest + age + male + country, data =data, method = 'logistic')

## approach would encourage drug use
m_scen1_arrest_encour <- polr(scen1supp ~ scen1encour * arrest + age + male + country, data =data, method = 'logistic')
m_scen2_arrest_encour <- polr(scen2supp ~ scen2encour * arrest + age + male + country, data =data, method = 'logistic')
m_scen3_arrest_encour <- polr(scen3supp ~ scen3encour * arrest + age + male + country, data =data, method = 'logistic')
m_scen4_arrest_encour <- polr(scen4supp ~ scen4encour * arrest + age + male + country, data =data, method = 'logistic')
m_scen5_arrest_encour <- polr(scen5supp ~ scen5encour * arrest + age + male + country, data =data, method = 'logistic')
m_scen6_arrest_encour <- polr(scen6supp ~ scen6encour * arrest + age + male + country, data =data, method = 'logistic')

## approach would restrict freedom
m_scen1_arrest_free <- polr(scen1supp ~ scen1free * arrest + age + male + country, data =data, method = 'logistic')
m_scen2_arrest_free <- polr(scen2supp ~ scen2free * arrest + age + male + country, data =data, method = 'logistic')
m_scen3_arrest_free <- polr(scen3supp ~ scen3free * arrest + age + male + country, data =data, method = 'logistic')
m_scen4_arrest_free <- polr(scen4supp ~ scen4free * arrest + age + male + country, data =data, method = 'logistic')
m_scen5_arrest_free <- polr(scen5supp ~ scen5free * arrest + age + male + country, data =data, method = 'logistic')
m_scen6_arrest_free <- polr(scen6supp ~ scen6free * arrest + age + male + country, data =data, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_arrest_health <- polr(scen1supp ~ scen1health * arrest + age + male + country, data =data, method = 'logistic')
m_scen2_arrest_health <- polr(scen2supp ~ scen2health * arrest + age + male + country, data =data, method = 'logistic')
m_scen3_arrest_health <- polr(scen3supp ~ scen3health * arrest + age + male + country, data =data, method = 'logistic')
m_scen4_arrest_health <- polr(scen4supp ~ scen4health * arrest + age + male + country, data =data, method = 'logistic')
m_scen5_arrest_health <- polr(scen5supp ~ scen5health * arrest + age + male + country, data =data, method = 'logistic')
m_scen6_arrest_health <- polr(scen6supp ~ scen6health * arrest + age + male + country, data =data, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_arrest_vulner <- polr(scen1supp ~ scen1vulner * arrest + age + male + country, data =data, method = 'logistic')
m_scen2_arrest_vulner <- polr(scen2supp ~ scen2vulner * arrest + age + male + country, data =data, method = 'logistic')
m_scen3_arrest_vulner <- polr(scen3supp ~ scen3vulner * arrest + age + male + country, data =data, method = 'logistic')
m_scen4_arrest_vulner <- polr(scen4supp ~ scen4vulner * arrest + age + male + country, data =data, method = 'logistic')
m_scen5_arrest_vulner <- polr(scen5supp ~ scen5vulner * arrest + age + male + country, data =data, method = 'logistic')
m_scen6_arrest_vulner <- polr(scen6supp ~ scen6vulner * arrest + age + male + country, data =data, method = 'logistic')

## would boost the economy
m_scen1_arrest_econ <- polr(scen1supp ~ scen1econ * arrest + age + male + country, data =data, method = 'logistic')
m_scen2_arrest_econ <- polr(scen2supp ~ scen2econ * arrest + age + male + country, data =data, method = 'logistic')
m_scen3_arrest_econ <- polr(scen3supp ~ scen3econ * arrest + age + male + country, data =data, method = 'logistic')
m_scen4_arrest_econ <- polr(scen4supp ~ scen4econ * arrest + age + male + country, data =data, method = 'logistic')
m_scen5_arrest_econ <- polr(scen5supp ~ scen5econ * arrest + age + male + country, data =data, method = 'logistic')
m_scen6_arrest_econ <- polr(scen6supp ~ scen6econ * arrest + age + male + country, data =data, method = 'logistic')

## would improve trust in govt
m_scen1_arrest_trust <- polr(scen1supp ~ scen1trust * arrest + age + male + country, data =data, method = 'logistic')
m_scen2_arrest_trust <- polr(scen2supp ~ scen2trust * arrest + age + male + country, data =data, method = 'logistic')
m_scen3_arrest_trust <- polr(scen3supp ~ scen3trust * arrest + age + male + country, data =data, method = 'logistic')
m_scen4_arrest_trust <- polr(scen4supp ~ scen4trust * arrest + age + male + country, data =data, method = 'logistic')
m_scen5_arrest_trust <- polr(scen5supp ~ scen5trust * arrest + age + male + country, data =data, method = 'logistic')
m_scen6_arrest_trust <- polr(scen6supp ~ scen6trust * arrest + age + male + country, data =data, method = 'logistic')


models_arrest <- list(m_scen1_arrest_crime, m_scen2_arrest_crime, m_scen3_arrest_crime, m_scen4_arrest_crime, m_scen5_arrest_crime, m_scen6_arrest_crime,
                          m_scen1_arrest_encour, m_scen2_arrest_encour, m_scen3_arrest_encour, m_scen4_arrest_encour, m_scen5_arrest_encour, m_scen6_arrest_encour,
                          m_scen1_arrest_free, m_scen2_arrest_free, m_scen3_arrest_free, m_scen4_arrest_free, m_scen5_arrest_free, m_scen6_arrest_free,
                          m_scen1_arrest_health, m_scen2_arrest_health, m_scen3_arrest_health, m_scen4_arrest_health, m_scen5_arrest_health, m_scen6_arrest_health,
                          m_scen1_arrest_vulner, m_scen2_arrest_vulner, m_scen3_arrest_vulner, m_scen4_arrest_vulner, m_scen5_arrest_vulner, m_scen6_arrest_vulner,
                          m_scen1_arrest_econ, m_scen2_arrest_econ, m_scen3_arrest_econ, m_scen4_arrest_econ, m_scen5_arrest_econ, m_scen6_arrest_econ,
                          m_scen1_arrest_trust, m_scen2_arrest_trust, m_scen3_arrest_trust, m_scen4_arrest_trust, m_scen5_arrest_trust, m_scen6_arrest_trust)


# Apply the function to each model in the list and bind rows
coefficients_data_arrest <- lapply(models_arrest, extract_coefficients) %>%
  bind_rows() %>%
  mutate(mor = "Never arrested",
         scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))

# changing ref cat
data_arrest <- data %>% 
  mutate(arrest_new = case_when(
    arrest == F ~ "never arrested",
    arrest == T ~ "been arrested"
  )) %>%
  mutate(arrest_new = factor(arrest_new, levels = c("been arrested", "never arrested")))

## aproach would reduce crime
m_scen1_arrest_new_crime <- polr(scen1supp ~ scen1crime * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen2_arrest_new_crime <- polr(scen2supp ~ scen2crime * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen3_arrest_new_crime <- polr(scen3supp ~ scen3crime * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen4_arrest_new_crime <- polr(scen4supp ~ scen4crime * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen5_arrest_new_crime <- polr(scen5supp ~ scen5crime * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen6_arrest_new_crime <- polr(scen6supp ~ scen6crime * arrest_new + age + male + country, data =data_arrest, method = 'logistic')

## approach would encourage drug use
m_scen1_arrest_new_encour <- polr(scen1supp ~ scen1encour * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen2_arrest_new_encour <- polr(scen2supp ~ scen2encour * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen3_arrest_new_encour <- polr(scen3supp ~ scen3encour * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen4_arrest_new_encour <- polr(scen4supp ~ scen4encour * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen5_arrest_new_encour <- polr(scen5supp ~ scen5encour * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen6_arrest_new_encour <- polr(scen6supp ~ scen6encour * arrest_new + age + male + country, data =data_arrest, method = 'logistic')

## approach would restrict freedom
m_scen1_arrest_new_free <- polr(scen1supp ~ scen1free * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen2_arrest_new_free <- polr(scen2supp ~ scen2free * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen3_arrest_new_free <- polr(scen3supp ~ scen3free * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen4_arrest_new_free <- polr(scen4supp ~ scen4free * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen5_arrest_new_free <- polr(scen5supp ~ scen5free * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen6_arrest_new_free <- polr(scen6supp ~ scen6free * arrest_new + age + male + country, data =data_arrest, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_arrest_new_health <- polr(scen1supp ~ scen1health * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen2_arrest_new_health <- polr(scen2supp ~ scen2health * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen3_arrest_new_health <- polr(scen3supp ~ scen3health * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen4_arrest_new_health <- polr(scen4supp ~ scen4health * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen5_arrest_new_health <- polr(scen5supp ~ scen5health * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen6_arrest_new_health <- polr(scen6supp ~ scen6health * arrest_new + age + male + country, data =data_arrest, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_arrest_new_vulner <- polr(scen1supp ~ scen1vulner * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen2_arrest_new_vulner <- polr(scen2supp ~ scen2vulner * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen3_arrest_new_vulner <- polr(scen3supp ~ scen3vulner * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen4_arrest_new_vulner <- polr(scen4supp ~ scen4vulner * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen5_arrest_new_vulner <- polr(scen5supp ~ scen5vulner * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen6_arrest_new_vulner <- polr(scen6supp ~ scen6vulner * arrest_new + age + male + country, data =data_arrest, method = 'logistic')

## would boost the economy
m_scen1_arrest_new_econ <- polr(scen1supp ~ scen1econ * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen2_arrest_new_econ <- polr(scen2supp ~ scen2econ * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen3_arrest_new_econ <- polr(scen3supp ~ scen3econ * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen4_arrest_new_econ <- polr(scen4supp ~ scen4econ * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen5_arrest_new_econ <- polr(scen5supp ~ scen5econ * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen6_arrest_new_econ <- polr(scen6supp ~ scen6econ * arrest_new + age + male + country, data =data_arrest, method = 'logistic')

## would improve trust in govt
m_scen1_arrest_new_trust <- polr(scen1supp ~ scen1trust * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen2_arrest_new_trust <- polr(scen2supp ~ scen2trust * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen3_arrest_new_trust <- polr(scen3supp ~ scen3trust * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen4_arrest_new_trust <- polr(scen4supp ~ scen4trust * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen5_arrest_new_trust <- polr(scen5supp ~ scen5trust * arrest_new + age + male + country, data =data_arrest, method = 'logistic')
m_scen6_arrest_new_trust <- polr(scen6supp ~ scen6trust * arrest_new + age + male + country, data =data_arrest, method = 'logistic')


models_arrest_new <- list(m_scen1_arrest_new_crime, m_scen2_arrest_new_crime, m_scen3_arrest_new_crime, m_scen4_arrest_new_crime, m_scen5_arrest_new_crime, m_scen6_arrest_new_crime,
                          m_scen1_arrest_new_encour, m_scen2_arrest_new_encour, m_scen3_arrest_new_encour, m_scen4_arrest_new_encour, m_scen5_arrest_new_encour, m_scen6_arrest_new_encour,
                          m_scen1_arrest_new_free, m_scen2_arrest_new_free, m_scen3_arrest_new_free, m_scen4_arrest_new_free, m_scen5_arrest_new_free, m_scen6_arrest_new_free,
                          m_scen1_arrest_new_health, m_scen2_arrest_new_health, m_scen3_arrest_new_health, m_scen4_arrest_new_health, m_scen5_arrest_new_health, m_scen6_arrest_new_health,
                          m_scen1_arrest_new_vulner, m_scen2_arrest_new_vulner, m_scen3_arrest_new_vulner, m_scen4_arrest_new_vulner, m_scen5_arrest_new_vulner, m_scen6_arrest_new_vulner,
                          m_scen1_arrest_new_econ, m_scen2_arrest_new_econ, m_scen3_arrest_new_econ, m_scen4_arrest_new_econ, m_scen5_arrest_new_econ, m_scen6_arrest_new_econ,
                          m_scen1_arrest_new_trust, m_scen2_arrest_new_trust, m_scen3_arrest_new_trust, m_scen4_arrest_new_trust, m_scen5_arrest_new_trust, m_scen6_arrest_new_trust)


# Apply the function to each model in the list and bind rows
coefficients_data_arrest_new <- lapply(models_arrest_new, extract_coefficients) %>%
  bind_rows() %>%
  mutate(mor = "Been arrested",
         scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))



# ref: never arrested
## aproach would reduce crime
m_scen1_purchase_crime <- polr(scen1supp ~ scen1crime * purchase + age + male + country, data =data, method = 'logistic')
m_scen2_purchase_crime <- polr(scen2supp ~ scen2crime * purchase + age + male + country, data =data, method = 'logistic')
m_scen3_purchase_crime <- polr(scen3supp ~ scen3crime * purchase + age + male + country, data =data, method = 'logistic')
m_scen4_purchase_crime <- polr(scen4supp ~ scen4crime * purchase + age + male + country, data =data, method = 'logistic')
m_scen5_purchase_crime <- polr(scen5supp ~ scen5crime * purchase + age + male + country, data =data, method = 'logistic')
m_scen6_purchase_crime <- polr(scen6supp ~ scen6crime * purchase + age + male + country, data =data, method = 'logistic')

## approach would encourage drug use
m_scen1_purchase_encour <- polr(scen1supp ~ scen1encour * purchase + age + male + country, data =data, method = 'logistic')
m_scen2_purchase_encour <- polr(scen2supp ~ scen2encour * purchase + age + male + country, data =data, method = 'logistic')
m_scen3_purchase_encour <- polr(scen3supp ~ scen3encour * purchase + age + male + country, data =data, method = 'logistic')
m_scen4_purchase_encour <- polr(scen4supp ~ scen4encour * purchase + age + male + country, data =data, method = 'logistic')
m_scen5_purchase_encour <- polr(scen5supp ~ scen5encour * purchase + age + male + country, data =data, method = 'logistic')
m_scen6_purchase_encour <- polr(scen6supp ~ scen6encour * purchase + age + male + country, data =data, method = 'logistic')

## approach would restrict freedom
m_scen1_purchase_free <- polr(scen1supp ~ scen1free * purchase + age + male + country, data =data, method = 'logistic')
m_scen2_purchase_free <- polr(scen2supp ~ scen2free * purchase + age + male + country, data =data, method = 'logistic')
m_scen3_purchase_free <- polr(scen3supp ~ scen3free * purchase + age + male + country, data =data, method = 'logistic')
m_scen4_purchase_free <- polr(scen4supp ~ scen4free * purchase + age + male + country, data =data, method = 'logistic')
m_scen5_purchase_free <- polr(scen5supp ~ scen5free * purchase + age + male + country, data =data, method = 'logistic')
m_scen6_purchase_free <- polr(scen6supp ~ scen6free * purchase + age + male + country, data =data, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_purchase_health <- polr(scen1supp ~ scen1health * purchase + age + male + country, data =data, method = 'logistic')
m_scen2_purchase_health <- polr(scen2supp ~ scen2health * purchase + age + male + country, data =data, method = 'logistic')
m_scen3_purchase_health <- polr(scen3supp ~ scen3health * purchase + age + male + country, data =data, method = 'logistic')
m_scen4_purchase_health <- polr(scen4supp ~ scen4health * purchase + age + male + country, data =data, method = 'logistic')
m_scen5_purchase_health <- polr(scen5supp ~ scen5health * purchase + age + male + country, data =data, method = 'logistic')
m_scen6_purchase_health <- polr(scen6supp ~ scen6health * purchase + age + male + country, data =data, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_purchase_vulner <- polr(scen1supp ~ scen1vulner * purchase + age + male + country, data =data, method = 'logistic')
m_scen2_purchase_vulner <- polr(scen2supp ~ scen2vulner * purchase + age + male + country, data =data, method = 'logistic')
m_scen3_purchase_vulner <- polr(scen3supp ~ scen3vulner * purchase + age + male + country, data =data, method = 'logistic')
m_scen4_purchase_vulner <- polr(scen4supp ~ scen4vulner * purchase + age + male + country, data =data, method = 'logistic')
m_scen5_purchase_vulner <- polr(scen5supp ~ scen5vulner * purchase + age + male + country, data =data, method = 'logistic')
m_scen6_purchase_vulner <- polr(scen6supp ~ scen6vulner * purchase + age + male + country, data =data, method = 'logistic')

## would boost the economy
m_scen1_purchase_econ <- polr(scen1supp ~ scen1econ * purchase + age + male + country, data =data, method = 'logistic')
m_scen2_purchase_econ <- polr(scen2supp ~ scen2econ * purchase + age + male + country, data =data, method = 'logistic')
m_scen3_purchase_econ <- polr(scen3supp ~ scen3econ * purchase + age + male + country, data =data, method = 'logistic')
m_scen4_purchase_econ <- polr(scen4supp ~ scen4econ * purchase + age + male + country, data =data, method = 'logistic')
m_scen5_purchase_econ <- polr(scen5supp ~ scen5econ * purchase + age + male + country, data =data, method = 'logistic')
m_scen6_purchase_econ <- polr(scen6supp ~ scen6econ * purchase + age + male + country, data =data, method = 'logistic')

## would improve trust in govt
m_scen1_purchase_trust <- polr(scen1supp ~ scen1trust * purchase + age + male + country, data =data, method = 'logistic')
m_scen2_purchase_trust <- polr(scen2supp ~ scen2trust * purchase + age + male + country, data =data, method = 'logistic')
m_scen3_purchase_trust <- polr(scen3supp ~ scen3trust * purchase + age + male + country, data =data, method = 'logistic')
m_scen4_purchase_trust <- polr(scen4supp ~ scen4trust * purchase + age + male + country, data =data, method = 'logistic')
m_scen5_purchase_trust <- polr(scen5supp ~ scen5trust * purchase + age + male + country, data =data, method = 'logistic')
m_scen6_purchase_trust <- polr(scen6supp ~ scen6trust * purchase + age + male + country, data =data, method = 'logistic')

models_purchase <- list(m_scen1_purchase_crime, m_scen2_purchase_crime, m_scen3_purchase_crime, m_scen4_purchase_crime, m_scen5_purchase_crime, m_scen6_purchase_crime,
                        m_scen1_purchase_encour, m_scen2_purchase_encour, m_scen3_purchase_encour, m_scen4_purchase_encour, m_scen5_purchase_encour, m_scen6_purchase_encour,
                        m_scen1_purchase_free, m_scen2_purchase_free, m_scen3_purchase_free, m_scen4_purchase_free, m_scen5_purchase_free, m_scen6_purchase_free,
                        m_scen1_purchase_health, m_scen2_purchase_health, m_scen3_purchase_health, m_scen4_purchase_health, m_scen5_purchase_health, m_scen6_purchase_health,
                        m_scen1_purchase_vulner, m_scen2_purchase_vulner, m_scen3_purchase_vulner, m_scen4_purchase_vulner, m_scen5_purchase_vulner, m_scen6_purchase_vulner,
                        m_scen1_purchase_econ, m_scen2_purchase_econ, m_scen3_purchase_econ, m_scen4_purchase_econ, m_scen5_purchase_econ, m_scen6_purchase_econ,
                        m_scen1_purchase_trust, m_scen2_purchase_trust, m_scen3_purchase_trust, m_scen4_purchase_trust, m_scen5_purchase_trust, m_scen6_purchase_trust)

data_notlast12 <- data %>%
  mutate(purchase = factor(purchase, levels = c("Yes, but not in the last 12 months", "Yes, in the last 12 months")))

## aproach would reduce crime
m_scen1_purchasenotlast12_crime <- polr(scen1supp ~ scen1crime * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen2_purchasenotlast12_crime <- polr(scen2supp ~ scen2crime * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen3_purchasenotlast12_crime <- polr(scen3supp ~ scen3crime * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen4_purchasenotlast12_crime <- polr(scen4supp ~ scen4crime * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen5_purchasenotlast12_crime <- polr(scen5supp ~ scen5crime * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen6_purchasenotlast12_crime <- polr(scen6supp ~ scen6crime * purchase + age + male + country, data =data_notlast12, method = 'logistic')

## approach would encourage drug use
m_scen1_purchasenotlast12_encour <- polr(scen1supp ~ scen1encour * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen2_purchasenotlast12_encour <- polr(scen2supp ~ scen2encour * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen3_purchasenotlast12_encour <- polr(scen3supp ~ scen3encour * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen4_purchasenotlast12_encour <- polr(scen4supp ~ scen4encour * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen5_purchasenotlast12_encour <- polr(scen5supp ~ scen5encour * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen6_purchasenotlast12_encour <- polr(scen6supp ~ scen6encour * purchase + age + male + country, data =data_notlast12, method = 'logistic')

## approach would restrict freedom
m_scen1_purchasenotlast12_free <- polr(scen1supp ~ scen1free * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen2_purchasenotlast12_free <- polr(scen2supp ~ scen2free * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen3_purchasenotlast12_free <- polr(scen3supp ~ scen3free * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen4_purchasenotlast12_free <- polr(scen4supp ~ scen4free * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen5_purchasenotlast12_free <- polr(scen5supp ~ scen5free * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen6_purchasenotlast12_free <- polr(scen6supp ~ scen6free * purchase + age + male + country, data =data_notlast12, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_purchasenotlast12_health <- polr(scen1supp ~ scen1health * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen2_purchasenotlast12_health <- polr(scen2supp ~ scen2health * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen3_purchasenotlast12_health <- polr(scen3supp ~ scen3health * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen4_purchasenotlast12_health <- polr(scen4supp ~ scen4health * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen5_purchasenotlast12_health <- polr(scen5supp ~ scen5health * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen6_purchasenotlast12_health <- polr(scen6supp ~ scen6health * purchase + age + male + country, data =data_notlast12, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_purchasenotlast12_vulner <- polr(scen1supp ~ scen1vulner * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen2_purchasenotlast12_vulner <- polr(scen2supp ~ scen2vulner * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen3_purchasenotlast12_vulner <- polr(scen3supp ~ scen3vulner * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen4_purchasenotlast12_vulner <- polr(scen4supp ~ scen4vulner * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen5_purchasenotlast12_vulner <- polr(scen5supp ~ scen5vulner * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen6_purchasenotlast12_vulner <- polr(scen6supp ~ scen6vulner * purchase + age + male + country, data =data_notlast12, method = 'logistic')

## would boost the economy
m_scen1_purchasenotlast12_econ <- polr(scen1supp ~ scen1econ * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen2_purchasenotlast12_econ <- polr(scen2supp ~ scen2econ * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen3_purchasenotlast12_econ <- polr(scen3supp ~ scen3econ * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen4_purchasenotlast12_econ <- polr(scen4supp ~ scen4econ * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen5_purchasenotlast12_econ <- polr(scen5supp ~ scen5econ * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen6_purchasenotlast12_econ <- polr(scen6supp ~ scen6econ * purchase + age + male + country, data =data_notlast12, method = 'logistic')

## would improve trust in govt
m_scen1_purchasenotlast12_trust <- polr(scen1supp ~ scen1trust * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen2_purchasenotlast12_trust <- polr(scen2supp ~ scen2trust * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen3_purchasenotlast12_trust <- polr(scen3supp ~ scen3trust * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen4_purchasenotlast12_trust <- polr(scen4supp ~ scen4trust * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen5_purchasenotlast12_trust <- polr(scen5supp ~ scen5trust * purchase + age + male + country, data =data_notlast12, method = 'logistic')
m_scen6_purchasenotlast12_trust <- polr(scen6supp ~ scen6trust * purchase + age + male + country, data =data_notlast12, method = 'logistic')

models_purchasenotlast12 <- list(m_scen1_purchasenotlast12_crime, m_scen2_purchasenotlast12_crime, m_scen3_purchasenotlast12_crime, m_scen4_purchasenotlast12_crime, m_scen5_purchasenotlast12_crime, m_scen6_purchasenotlast12_crime,
                                 m_scen1_purchasenotlast12_encour, m_scen2_purchasenotlast12_encour, m_scen3_purchasenotlast12_encour, m_scen4_purchasenotlast12_encour, m_scen5_purchasenotlast12_encour, m_scen6_purchasenotlast12_encour,
                                 m_scen1_purchasenotlast12_free, m_scen2_purchasenotlast12_free, m_scen3_purchasenotlast12_free, m_scen4_purchasenotlast12_free, m_scen5_purchasenotlast12_free, m_scen6_purchasenotlast12_free,
                                 m_scen1_purchasenotlast12_health, m_scen2_purchasenotlast12_health, m_scen3_purchasenotlast12_health, m_scen4_purchasenotlast12_health, m_scen5_purchasenotlast12_health, m_scen6_purchasenotlast12_health,
                                 m_scen1_purchasenotlast12_vulner, m_scen2_purchasenotlast12_vulner, m_scen3_purchasenotlast12_vulner, m_scen4_purchasenotlast12_vulner, m_scen5_purchasenotlast12_vulner, m_scen6_purchasenotlast12_vulner,
                                 m_scen1_purchasenotlast12_econ, m_scen2_purchasenotlast12_econ, m_scen3_purchasenotlast12_econ, m_scen4_purchasenotlast12_econ, m_scen5_purchasenotlast12_econ, m_scen6_purchasenotlast12_econ,
                                 m_scen1_purchasenotlast12_trust, m_scen2_purchasenotlast12_trust, m_scen3_purchasenotlast12_trust, m_scen4_purchasenotlast12_trust, m_scen5_purchasenotlast12_trust, m_scen6_purchasenotlast12_trust)


data_last12 <- data %>%
  mutate(purchase = factor(purchase, levels = c("Yes, in the last 12 months", "Yes, but not in the last 12 months", "No, never")))

## aproach would reduce crime
m_scen1_purchaselast12_crime <- polr(scen1supp ~ scen1crime * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen2_purchaselast12_crime <- polr(scen2supp ~ scen2crime * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen3_purchaselast12_crime <- polr(scen3supp ~ scen3crime * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen4_purchaselast12_crime <- polr(scen4supp ~ scen4crime * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen5_purchaselast12_crime <- polr(scen5supp ~ scen5crime * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen6_purchaselast12_crime <- polr(scen6supp ~ scen6crime * purchase + age + male + country, data =data_last12, method = 'logistic')

## approach would encourage drug use
m_scen1_purchaselast12_encour <- polr(scen1supp ~ scen1encour * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen2_purchaselast12_encour <- polr(scen2supp ~ scen2encour * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen3_purchaselast12_encour <- polr(scen3supp ~ scen3encour * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen4_purchaselast12_encour <- polr(scen4supp ~ scen4encour * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen5_purchaselast12_encour <- polr(scen5supp ~ scen5encour * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen6_purchaselast12_encour <- polr(scen6supp ~ scen6encour * purchase + age + male + country, data =data_last12, method = 'logistic')

## approach would restrict freedom
m_scen1_purchaselast12_free <- polr(scen1supp ~ scen1free * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen2_purchaselast12_free <- polr(scen2supp ~ scen2free * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen3_purchaselast12_free <- polr(scen3supp ~ scen3free * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen4_purchaselast12_free <- polr(scen4supp ~ scen4free * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen5_purchaselast12_free <- polr(scen5supp ~ scen5free * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen6_purchaselast12_free <- polr(scen6supp ~ scen6free * purchase + age + male + country, data =data_last12, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_purchaselast12_health <- polr(scen1supp ~ scen1health * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen2_purchaselast12_health <- polr(scen2supp ~ scen2health * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen3_purchaselast12_health <- polr(scen3supp ~ scen3health * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen4_purchaselast12_health <- polr(scen4supp ~ scen4health * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen5_purchaselast12_health <- polr(scen5supp ~ scen5health * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen6_purchaselast12_health <- polr(scen6supp ~ scen6health * purchase + age + male + country, data =data_last12, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_purchaselast12_vulner <- polr(scen1supp ~ scen1vulner * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen2_purchaselast12_vulner <- polr(scen2supp ~ scen2vulner * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen3_purchaselast12_vulner <- polr(scen3supp ~ scen3vulner * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen4_purchaselast12_vulner <- polr(scen4supp ~ scen4vulner * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen5_purchaselast12_vulner <- polr(scen5supp ~ scen5vulner * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen6_purchaselast12_vulner <- polr(scen6supp ~ scen6vulner * purchase + age + male + country, data =data_last12, method = 'logistic')

## would boost the economy
m_scen1_purchaselast12_econ <- polr(scen1supp ~ scen1econ * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen2_purchaselast12_econ <- polr(scen2supp ~ scen2econ * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen3_purchaselast12_econ <- polr(scen3supp ~ scen3econ * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen4_purchaselast12_econ <- polr(scen4supp ~ scen4econ * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen5_purchaselast12_econ <- polr(scen5supp ~ scen5econ * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen6_purchaselast12_econ <- polr(scen6supp ~ scen6econ * purchase + age + male + country, data =data_last12, method = 'logistic')

## would improve trust in govt
m_scen1_purchaselast12_trust <- polr(scen1supp ~ scen1trust * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen2_purchaselast12_trust <- polr(scen2supp ~ scen2trust * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen3_purchaselast12_trust <- polr(scen3supp ~ scen3trust * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen4_purchaselast12_trust <- polr(scen4supp ~ scen4trust * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen5_purchaselast12_trust <- polr(scen5supp ~ scen5trust * purchase + age + male + country, data =data_last12, method = 'logistic')
m_scen6_purchaselast12_trust <- polr(scen6supp ~ scen6trust * purchase + age + male + country, data =data_last12, method = 'logistic')

models_purchaselast12 <- list(m_scen1_purchaselast12_crime, m_scen2_purchaselast12_crime, m_scen3_purchaselast12_crime, m_scen4_purchaselast12_crime, m_scen5_purchaselast12_crime, m_scen6_purchaselast12_crime,
                              m_scen1_purchaselast12_encour, m_scen2_purchaselast12_encour, m_scen3_purchaselast12_encour, m_scen4_purchaselast12_encour, m_scen5_purchaselast12_encour, m_scen6_purchaselast12_encour,
                              m_scen1_purchaselast12_free, m_scen2_purchaselast12_free, m_scen3_purchaselast12_free, m_scen4_purchaselast12_free, m_scen5_purchaselast12_free, m_scen6_purchaselast12_free,
                              m_scen1_purchaselast12_health, m_scen2_purchaselast12_health, m_scen3_purchaselast12_health, m_scen4_purchaselast12_health, m_scen5_purchaselast12_health, m_scen6_purchaselast12_health,
                              m_scen1_purchaselast12_vulner, m_scen2_purchaselast12_vulner, m_scen3_purchaselast12_vulner, m_scen4_purchaselast12_vulner, m_scen5_purchaselast12_vulner, m_scen6_purchaselast12_vulner,
                              m_scen1_purchaselast12_econ, m_scen2_purchaselast12_econ, m_scen3_purchaselast12_econ, m_scen4_purchaselast12_econ, m_scen5_purchaselast12_econ, m_scen6_purchaselast12_econ,
                              m_scen1_purchaselast12_trust, m_scen2_purchaselast12_trust, m_scen3_purchaselast12_trust, m_scen4_purchaselast12_trust, m_scen5_purchaselast12_trust, m_scen6_purchaselast12_trust)


# Apply the function to each model in the list and bind rows
coefficients_data_purchase <- lapply(models_purchase, extract_coefficients) %>%
  bind_rows() %>%
  mutate(mor = "Never purchased drugs",
         scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))

coefficients_data_purchase_notlast12 <- lapply(models_purchasenotlast12, extract_coefficients) %>%
  bind_rows() %>%
  mutate(mor = "Purchased drugs, but not in the last 12 months",
         scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))

coefficients_data_purchaselast12 <- lapply(models_purchaselast12, extract_coefficients) %>%
  bind_rows() %>%
  mutate(mor = "Purchased drugs in the last 12 months",
         scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))

dataplot_all <-
  coefficients_data_immoral %>%
  bind_rows(coefficients_data_neutral) %>%
  bind_rows(coefficients_data_moral) %>%
  mutate(mod = 'morality') %>%
  bind_rows(coefficients_data_arrest %>% mutate(mod = 'arrest')) %>%
  bind_rows(coefficients_data_arrest_new %>% mutate(mod = 'arrest')) %>%
  bind_rows(coefficients_data_purchase %>% mutate(mod = 'purchase')) %>%
  bind_rows(coefficients_data_purchase_notlast12 %>% mutate(mod = 'purchase')) %>%
  bind_rows(coefficients_data_purchaselast12 %>% mutate(mod = 'purchase'))

saveRDS(dataplot_all, "data/dataplot.RDS")

